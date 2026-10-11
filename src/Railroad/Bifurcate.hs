{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- The 'Bifurcate' class, which splits a structure (@Bool@, @Maybe@, @Either@, @Validation@,
-- traversables of them) into a failure and a success case, and the operators that never
-- throw. The throwing operators are in "Railroad" and "Railroad.MonadError".
module Railroad.Bifurcate
  ( Bifurcate (..), CErr, CRes
  , (?>), (??~), (?~), (?|), (?|<>)
  ) where

import           Data.Bifunctor  (first)
import           Data.Bool       (bool)
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
-- INCOHERENT so the base instance wins and the outer layer is peeled first (@m ? e1 ? e2@).
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


-- | Tags the value: @Right@ if it satisfies the predicate, else @Left@ carrying it. Finish
-- with @?@ or @??@ (the mapper sees the rejected value): @m ?> p ? err@.
(?>) :: forall m a. Functor m => m a -> (a -> Bool) -> m (Either a a)
action ?> predicate = (\x -> if predicate x then Right x else Left x) <$> action

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

infixl 1 ??~, ?~, ?|, ?|<>, ?>
