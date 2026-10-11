{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Railway-oriented operators: turn a failure in a 'Bifurcate' structure (@Bool@, @Maybe@,
-- @Either@, @Validation@, traversables of them) or a bad cardinality into a throw in the
-- @Error@ effect, keeping the happy path clean. For @MonadError@ use "Railroad.MonadError".
module Railroad where

import           Data.Bool               (bool)
import           Data.Foldable           (toList)
import           Data.Kind               (Type)
import           Data.Validation         (Validation (..))
import           Effectful               (Eff, type (:>))
import           Effectful.Error.Dynamic (Error, throwError_)


-- | What the failure case carries: @()@ for 'Bool' and 'Maybe', @e@ for 'Either' and
-- 'Validation', that of the element for a traversable.
type family CErr f :: Type where
  CErr Bool               = ()
  CErr (Maybe a)          = ()
  CErr (Either e a)       = e
  CErr (Validation e a)   = e
  CErr (t a)              = CErr a
-- | What the success case carries: @()@ for 'Bool', @a@ for 'Maybe', 'Either' and
-- 'Validation', @t (CRes a)@ for a traversable @t a@.
type family CRes f :: Type where
  CRes Bool               = ()
  CRes (Maybe a)          = a
  CRes (Either e a)       = a
  CRes (Validation e a)   = a
  CRes (t a)              = t (CRes a)

-- | A catamorphism to Either
class Bifurcate f where
  bifurcate :: f -> Either (CErr f) (CRes f)
instance Bifurcate Bool where
  bifurcate = bool (Left ()) (Right ())
  -- bool   :: b -> b -> Bool -> b
instance Bifurcate (Maybe a) where
  bifurcate = maybe (Left ()) Right
  -- maybe  :: b -> (a -> b) -> Maybe a -> b
instance Bifurcate (Either e a) where
  bifurcate = id
  -- either :: (e -> b) -> (a -> b) -> Either e a -> b
instance Bifurcate (Validation e a) where
  bifurcate = \case
    Failure e -> Left e
    Success a -> Right a
  -- validation :: (e -> b) -> (a -> b) -> Validation e a -> b

instance (Traversable t, CErr (t Bool) ~ (), CRes (t Bool) ~ t ())
    => Bifurcate (t Bool) where
  bifurcate = bifurcate . sequenceA . fmap bifurcate
  -- Bool is not Applicative, so we need to map to Either first
instance (Traversable t, CErr (t (Maybe a)) ~ (), CRes (t (Maybe a)) ~ t a)
    => Bifurcate (t (Maybe a)) where
  bifurcate = bifurcate . sequenceA
instance (Traversable t, CErr (t (Either e a)) ~ e, CRes (t (Either e a)) ~ t a)
    => Bifurcate (t (Either e a)) where
  bifurcate = sequenceA
  -- pedagogically: bifurcate = bifurcate . sequenceA
instance (Traversable t, Semigroup e, CErr (t (Validation e a)) ~ e, CRes (t (Validation e a)) ~ t a)
    => Bifurcate (t (Validation e a)) where
  bifurcate = bifurcate . sequenceA

-- | Unwraps the success case, or throws the failure mapped by the function.
collapse :: (Error e :> es, Bifurcate f) => (CErr f -> e) -> f -> Eff es (CRes f)
collapse toErr = either (throwError_ . toErr) pure . bifurcate


-- | Unwraps the success case, or throws the error info mapped by the function.
(??) :: forall a es e. (Error e :> es, Bifurcate a) => Eff es a -> (CErr a -> e) -> Eff es (CRes a)
action ?? toErr = action >>= collapse toErr

-- | Unwraps the success case, or throws a constant error. Chains to peel nested layers:
-- @m ? e1 ? e2@.
(?) :: forall es e a. (Error e :> es, Bifurcate a) => Eff es a -> e -> Eff es (CRes a)
action ? err = action ?? const err

-- | Passes the value on if it satisfies the predicate, else throws the error built from
-- it: @(m ?> p) toErr@.
(?>) :: forall es e a. (Error e :> es) => Eff es a -> (a -> Bool) -> (a -> e) -> Eff es a
(?>) action predicate toErr = do
  val <- action
  if predicate val then pure val else throwError_ $ toErr val

-- | Unwraps the success case, or recovers with a value computed from the error info.
(??~) :: forall es a. Bifurcate a => Eff es a -> (CErr a -> CRes a) -> Eff es (CRes a)
action ??~ defaultFunc = action >>= either (pure . defaultFunc) pure . bifurcate

-- | Unwraps the success case, or recovers with a default value.
(?~) :: forall es a. Bifurcate a => Eff es a -> CRes a -> Eff es (CRes a)
action ?~ defaultVal = action ??~ (const defaultVal)


-- | Fallback: runs the right action only if the left fails. The sides may be different
-- structures with the same success type; the last error is kept. Finish with '?' or '??'.
--
-- > cache k ?| db k ?| legacy k ? NotFound
(?|) :: forall m a b. (Monad m, Bifurcate a, Bifurcate b, CRes a ~ CRes b)
     => m a -> m b -> m (Either (CErr b) (CRes a))
ma ?| mb = do
  a <- ma
  case bifurcate a of
    Right x -> pure (Right x)
    Left _  -> bifurcate <$> mb

-- | Like '?|', but if every source fails their errors are combined with '<>', in order.
-- The sides must also share the error type; map it to a common one first.
(?|<>) :: forall m a b. ( Monad m, Bifurcate a, Bifurcate b, CRes a ~ CRes b
                        , CErr a ~ CErr b, Semigroup (CErr a) )
       => m a -> m b -> m (Either (CErr a) (CRes a))
ma ?|<> mb = do
  a <- ma
  case bifurcate a of
    Right x  -> pure (Right x)
    Left ea  -> do
      b <- mb
      pure $ case bifurcate b of
        Right y  -> Right y
        Left eb  -> Left (ea <> eb)

-- | Why a collection was not a single element: empty, or too many (carrying it).
data CardinalityError ta = IsEmpty | TooMany ta

-- | Fold for 'CardinalityError': one result per case.
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty -> onEmpty
  TooMany xs -> onTooMany xs


-- | Succeeds if non-empty, returning the collection; else throws the constant error.
(?+) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> e -> Eff es (t a)
(?+) action err = do
  xs <- action
  if null xs then throwError_ err else pure xs


-- | Succeeds if there is exactly one element, returning it; else throws the error built
-- from the 'CardinalityError'.
(?!) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> (CardinalityError (t a) -> e) -> Eff es a
(?!) action toErr = do
  xs <- action
  case toList xs of
    []  -> throwError_ $ toErr IsEmpty
    [x] -> pure x
    _   -> throwError_ $ toErr $ TooMany xs


-- | Succeeds if empty, returning @()@; else throws the error built from the collection.
(?∅) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
(?∅) action toErr = do
  xs <- action
  if null xs then pure () else throwError_ (toErr xs)

-- | ASCII alias for '?∅'.
(?@) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
(?@) = (?∅)

infixl  0 ??, ?, ??~, ?~, ?!, ?+, ?∅, ?@, ?|, ?|<>
infixl 1 ?>
