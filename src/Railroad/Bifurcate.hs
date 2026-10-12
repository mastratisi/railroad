{-# LANGUAGE FlexibleContexts     #-}

-- |
-- The lifted layer: operators over any 'Functor' or 'Monad' that never throw, and the whole
-- pure layer of "Railroad.Classify", re-exported. The throwing operators are in "Railroad"
-- and "Railroad.MonadError".
module Railroad.Bifurcate
  ( module Railroad.Classify
  , (?>), (??~), (?~), (?|), (?|<>), (?<*)
  ) where

import           Data.Bifunctor    (first)
import           Railroad.Classify

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
