-- |
-- The operators of "Railroad" for any 'MonadError' (@mtl@, @transformers@, @ExceptT@,
-- servant's @Handler@) instead of the @Error@ effect. In an @effectful@ stack use "Railroad".
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Railroad.MonadError
  ( module Railroad.Bifurcate
  , module Railroad.Cardinality
  , collapse
  , (??), (?), (?+), (?!), (?∅), (?@)
  ) where

import           Control.Monad.Except (MonadError (..))
import           Data.Foldable        (toList)
import           Railroad.Bifurcate
import           Railroad.Cardinality

-- | Unwraps the success case, or throws the failure mapped by the function.
collapse :: (MonadError e m, Bifurcate f)
         => (CErr f -> e) -> f -> m (CRes f)
collapse toErr = either (throwError . toErr) pure . bifurcate

-- | Unwraps the success case, or throws the error info mapped by the function.
(??) :: forall a m e. (MonadError e m, Bifurcate a)
     => m a -> (CErr a -> e) -> m (CRes a)
action ?? toErr = action >>= collapse toErr

-- | Unwraps the success case, or throws a constant error. Chains to peel nested layers:
-- @m ? e1 ? e2@.
(?) :: forall m e a. (MonadError e m, Bifurcate a)
    => m a -> e -> m (CRes a)
action ? err = action ?? const err

-- | Succeeds if non-empty, returning the collection; else throws the constant error.
-- Chain it before @?@ on a traversable, which an empty collection passes.
(?+) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> e -> m (t a)
(?+) action err = do
  xs <- action
  if null xs then throwError err else pure xs

-- | Succeeds if there is exactly one element, returning it; else throws the error built
-- from the 'CardinalityError'.
(?!) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (CardinalityError (t a) -> e) -> m a
(?!) action toErr = do
  xs <- action
  case toList xs of
    []  -> throwError $ toErr IsEmpty
    [x] -> pure x
    _   -> throwError $ toErr $ TooMany xs

-- | Succeeds if empty, returning @()@; else throws the error built from the collection.
(?∅) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
(?∅) action toErr = do
  xs <- action
  if null xs then pure () else throwError (toErr xs)

-- | ASCII alias for '?∅'.
(?@) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
(?@) = (?∅)

infixl 1 ??, ?, ?!, ?+, ?∅, ?@
