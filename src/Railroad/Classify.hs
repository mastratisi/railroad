{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- The pure layer: the 'Bifurcate' class, which splits a structure (@Bool@, @Maybe@,
-- @Either@, @Validation@, traversables of them) into a failure and a success case, and the
-- classifiers built on it. Nothing here mentions a monad, a functor or a throw; the
-- operators that use them are in "Railroad.Bifurcate" (lifted, never throw) and in
-- "Railroad" and "Railroad.MonadError" (throwing).
module Railroad.Classify
  ( Bifurcate (..), CErr, CRes
  , neg, tagBy, partitionBifurcate, successes, failures, anySucceeds
  ) where

import           Data.Bool       (bool)
import           Data.Either     (partitionEithers)
import           Data.Foldable   (toList)
import           Data.Kind       (Type)
import           Data.Validation (Validation (..))


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
-- @neg . atLeastOne@ is @noneAtAll@ (both in "Railroad.Cardinality"), and @neg (neg x)@ is
-- @'bifurcate' x@.
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
