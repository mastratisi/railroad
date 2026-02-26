{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Railroad where

import           Data.Bool               (bool)
import           Data.Foldable           (toList)
import           Data.Kind               (Type)
import           Data.Validation         (Validation, toEither)
import           Effectful               (Eff, type (:>))
import           Effectful.Error.Dynamic (Error, throwError_)


type family CErr f :: Type where
  CErr Bool               = ()
  CErr (Maybe a)          = ()
  CErr (Either e a)       = e
  CErr (Validation e a)   = e
  CErr (t a)              = CErr a
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
  bifurcate = toEither
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
  bifurcate = toEither . sequenceA
  -- pedagogically: bifurcate = bifurcate . sequenceA

-- | Collapses a structure into its inner type or an effectful error
collapse :: (Error e :> es, Bifurcate f) => (CErr f -> e) -> f -> Eff es (CRes f)
collapse toErr = either (throwError_ . toErr) pure . bifurcate


-- | Collapses a structure using an error mapper.
(??) :: forall a es e. (Error e :> es, Bifurcate a) => Eff es a -> (CErr a -> e) -> Eff es (CRes a)
action ?? toErr = action >>= collapse toErr

-- | Collapses a structure using a constant error.
(?) :: forall es e a. (Error e :> es, Bifurcate a) => Eff es a -> e -> Eff es (CRes a)
action ? err = action ?? const err

-- | Collapses any value based on a predicate
(?>) :: forall es e a. (Error e :> es) => Eff es a -> (a -> Bool) -> (a -> e) -> Eff es a
(?>) action predicate toErr = do
  val <- action
  if predicate val then pure val else throwError_ $ toErr val

-- | Collapses a structure and recovers to a value dependent on the error
(??~) :: forall es a. Bifurcate a => Eff es a -> (CErr a -> CRes a) -> Eff es (CRes a)
action ??~ defaultFunc = action >>= either (pure . defaultFunc) pure . bifurcate

-- | Collapses a structure, and recovers to a constant default value in the error case
(?~) :: forall es a. Bifurcate a => Eff es a -> CRes a -> Eff es (CRes a)
action ?~ defaultVal = action ??~ (const defaultVal)


-- TODO: Derive instances for it? Functor?
data CardinalityError ta = IsEmpty | TooMany !ta

-- | Catamorphism for CardinalityError
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty -> onEmpty
  TooMany xs -> onTooMany xs


-- |  Non-empty is sucess state. Returns the collection as-is.
(?+) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> e -> Eff es (t a)
(?+) action err = do
  xs <- action
  if null xs then throwError_ err else pure xs


-- | Single element is sucess state.  Returns the single element
(?!) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> (CardinalityError (t a) -> e) -> Eff es a
(?!) action toErr = do
  xs <- action
  case toList xs of
    []  -> throwError_ $ toErr IsEmpty
    [x] -> pure x
    _   -> throwError_ $ toErr $ TooMany xs


-- | Empty is success state. Returns Unit.
(?∅) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
(?∅) action toErr = do
  xs <- action
  if null xs then pure () else throwError_ (toErr xs)

-- | Non-unicode alias for `(?∅)`
(?@) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
(?@) = (?∅)

infixl  0 ??, ?, ??~, ?~, ?!, ?+, ?∅, ?@
infixl 1 ?>


