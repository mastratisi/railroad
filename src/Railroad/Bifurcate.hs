{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- The 'Bifurcate' class, which splits a structure (@Bool@, @Maybe@, @Either@, @Validation@,
-- traversables of them) into a failure and a success case, the classifiers built on it, and
-- the operators that never throw. Nothing here depends on how a failure is thrown; the
-- throwing operators are in "Railroad" (@effectful@) and "Railroad.MonadError" (@mtl@).
module Railroad.Bifurcate
  ( -- * The class
    Bifurcate (..), CErr, CRes
    -- * Classifiers
    -- $classifiers
  , neg, tagBy, partitionBifurcate, successes, failures, anySucceeds
    -- * Cardinality
  , CardinalityError (..), cardinalityErr, atLeastOne, exactlyOne, noneAtAll
    -- * Operators that never throw
    -- $operators
  , (?>), (??~), (?~), (?|), (?|<>), (?<*)
  ) where

import           Data.Bifunctor  (first)
import           Data.Bool       (bool)
import           Data.Either     (partitionEithers)
import           Data.Foldable   (toList)
import           Data.Kind       (Type)
import           Data.Validation (Validation (..))

-- $classifiers
-- A classifier is a plain function into 'Either'. It needs no functor and never throws:
-- apply it with @\<&\>@ and finish with any operator, e.g. @xs \<&\> 'exactlyOne' ?? f@.

-- $operators
-- These work in any 'Functor' or 'Monad' and never throw. Finish a chain with @?@ or @??@
-- from "Railroad" or "Railroad.MonadError" to turn a remaining failure into a throw.

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
--
-- A traversable fails at its first failing element; an empty one succeeds (use @?+@ to
-- also require an element).
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

-- The traversable instances overlap the base ones for e.g. @Either e (Maybe a)@. They are
-- INCOHERENT so the base instance wins and the outer layer is unwrapped first (@m ? e1 ? e2@).
instance {-# INCOHERENT #-} (Traversable t, CErr (t Bool) ~ (), CRes (t Bool) ~ t ())
    => Bifurcate (t Bool) where
  bifurcate = bifurcate . sequenceA . fmap bifurcate
  -- Bool is not Applicative, so we need to map to Either first
instance {-# INCOHERENT #-} (Traversable t, CErr (t (Maybe a)) ~ (), CRes (t (Maybe a)) ~ t a)
    => Bifurcate (t (Maybe a)) where
  bifurcate = bifurcate . sequenceA
instance {-# INCOHERENT #-} (Traversable t, CErr (t (Either e a)) ~ e, CRes (t (Either e a)) ~ t a)
    => Bifurcate (t (Either e a)) where
  bifurcate = sequenceA
  -- pedagogically: bifurcate = bifurcate . sequenceA
instance {-# INCOHERENT #-} (Traversable t, Semigroup e, CErr (t (Validation e a)) ~ e, CRes (t (Validation e a)) ~ t a)
    => Bifurcate (t (Validation e a)) where
  bifurcate = bifurcate . sequenceA


-- | Negation: swaps failure and success. A verb after it acts on a value that was meant to
-- fail, and the error mapper sees what succeeded instead.
--
-- > lookupUser email <&> neg ?? const err409     -- Nothing passes, Just _ is a 409
--
-- On a traversable the failure is "some element failed", so @neg@ is the negation of the
-- all-succeed reading: @xs <&> neg ? e@ passes exactly when @xs ? e@ would throw.
-- @neg . atLeastOne@ is @noneAtAll@, and @neg (neg x)@ is @'bifurcate' x@.
neg :: Bifurcate a => a -> Either (CRes a) (CErr a)
neg = either Right Left . bifurcate

-- | The classifier behind @(?>)@: @Right@ if the value satisfies the predicate, else @Left@
-- carrying it.
tagBy :: (a -> Bool) -> a -> Either a a
tagBy predicate x = if predicate x then Right x else Left x

-- | Splits a collection of results into its failures and its successes, in order.
partitionBifurcate :: (Foldable t, Bifurcate a) => t a -> ([CErr a], [CRes a])
partitionBifurcate = partitionEithers . map bifurcate . toList

-- | The success values of a collection of results; the failures are dropped.
successes :: (Foldable t, Bifurcate a) => t a -> [CRes a]
successes = snd . partitionBifurcate

-- | The failure values of a collection of results.
failures :: (Foldable t, Bifurcate a) => t a -> [CErr a]
failures = fst . partitionBifurcate

-- | Classifies a collection of results as having at least one success. The success carries
-- every success value; the failure carries every failure value (none if the collection
-- was empty): @xs <&> anySucceeds ?? f@.
anySucceeds :: (Foldable t, Bifurcate a) => t a -> Either [CErr a] [CRes a]
anySucceeds xs = case partitionBifurcate xs of
  (es, [])  -> Left es
  (_, rs)   -> Right rs

-- | Why a collection was not a single element: empty, or too many (carrying it).
data CardinalityError ta = IsEmpty | TooMany ta
  deriving (Eq, Show)

-- | Fold for 'CardinalityError': one result per case.
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty -> onEmpty
  TooMany xs -> onTooMany xs

-- | Classifies a collection as non-empty. The failure carries nothing, so the result is
-- @Bifurcate@ with @()@ as the error info: @xs <&> atLeastOne ? e@ is @xs ?+ e@.
atLeastOne :: Foldable t => t a -> Maybe (t a)
atLeastOne xs = if null xs then Nothing else Just xs

-- | Classifies a collection as having exactly one element, returning it. The failure says
-- whether there were none or too many: @xs <&> exactlyOne ?? f@ is @xs ?! f@.
exactlyOne :: Foldable t => t a -> Either (CardinalityError (t a)) a
exactlyOne xs = case toList xs of
  []  -> Left IsEmpty
  [x] -> Right x
  _   -> Left (TooMany xs)

-- | Classifies a collection as empty. The failure carries the collection:
-- @xs <&> noneAtAll ?? f@ is @xs ?∅ f@.
noneAtAll :: Foldable t => t a -> Either (t a) ()
noneAtAll xs = if null xs then Right () else Left xs

-- | Tags the value: @Right@ if it satisfies the predicate, else @Left@ carrying it. Finish
-- with @?@ or @??@ (the mapper sees the rejected value): @m ?> p ? err@.
(?>) :: forall m a. Functor m => m a -> (a -> Bool) -> m (Either a a)
action ?> predicate = tagBy predicate <$> action

-- | Unwraps the success case, or recovers with a value computed from the error info.
(??~) :: forall m a. (Functor m, Bifurcate a) => m a -> (CErr a -> CRes a) -> m (CRes a)
action ??~ defaultFunc = either defaultFunc id . bifurcate <$> action

-- | Unwraps the success case, or recovers with a default value.
(?~) :: forall m a. (Functor m, Bifurcate a) => m a -> CRes a -> m (CRes a)
action ?~ defaultVal = action ??~ const defaultVal


-- | Fallback: runs the right action only if the left fails. The sides may be different
-- structures with the same success type; the last error is kept. Finish with @?@ or @??@.
--
-- > cache k ?| db k ?| legacy k ? NotFound
(?|) :: forall m a b. (Monad m, Bifurcate a, Bifurcate b, CRes a ~ CRes b)
     => m a -> m b -> m (Either (CErr b) (CRes a))
ma ?| mb = ma >>= either (const (bifurcate <$> mb)) (pure . Right) . bifurcate

-- | Like '?|', but if every source fails their errors are combined with '<>', in order.
-- The sides must also share the error type; map it to a common one first.
(?|<>) :: forall m a b. ( Monad m, Bifurcate a, Bifurcate b, CRes a ~ CRes b
                        , CErr a ~ CErr b, Semigroup (CErr a) )
       => m a -> m b -> m (Either (CErr a) (CRes a))
ma ?|<> mb = ma >>= either (\ea -> first (ea <>) . bifurcate <$> mb) (pure . Right) . bifurcate

-- | Runs an effect on the failure and passes the value through unchanged. Put it before @?@
-- or @??@ to log what they would discard: @m ?\<* logIt ? err@.
(?<*) :: forall m a. (Monad m, Bifurcate a) => m a -> (CErr a -> m ()) -> m a
action ?<* handler = action >>= \x -> x <$ either handler (const (pure ())) (bifurcate x)

infixl 1 ??~, ?~, ?|, ?|<>, ?>, ?<*
