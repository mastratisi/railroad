-- |
-- The operators of "Railroad" for any 'MonadError' (@mtl@, @transformers@, @ExceptT@,
-- servant's @Handler@) instead of the @Error@ effect. In an @effectful@ stack use "Railroad".
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Railroad.MonadError
  ( module Railroad          -- re-exports CErr, CRes, Bifurcate, CardinalityError, etc.
  , collapse
  , (??), (?), (?>), (??~), (?~), (?+), (?!), (?∅), (?@)
  ) where

import           Railroad             hiding (collapse, (?!), (?), (?+), (?>),
                                       (??), (??~), (?@), (?~), (?∅))

import           Control.Monad.Except (MonadError (..))
import           Data.Foldable        (toList)

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

-- | Passes the value on if it satisfies the predicate, else throws the error built from
-- it: @(m ?> p) toErr@.
(?>) :: forall m e a. (MonadError e m)
     => m a -> (a -> Bool) -> (a -> e) -> m a
(?>) action predicate toErr = do
  val <- action
  if predicate val then pure val else throwError $ toErr val

-- | Unwraps the success case, or recovers with a value computed from the error info.
(??~) :: forall m a. (Bifurcate a, Monad m) => m a -> (CErr a -> CRes a) -> m (CRes a)
action ??~ defaultFunc = action >>= either (pure . defaultFunc) pure . bifurcate

-- | Unwraps the success case, or recovers with a default value.
(?~) :: forall m a. (Bifurcate a, Monad m) => m a -> CRes a -> m (CRes a)
action ?~ defaultVal = action ??~ (const defaultVal)

-- | Succeeds if non-empty, returning the collection; else throws the constant error.
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

infixl  0 ??, ?, ??~, ?~, ?!, ?+, ?∅, ?@
infixl 1 ?>
