-- |
-- Re-implementation of Railroad operators ('??', '?', '?>', etc.)
-- using 'MonadError' / 'throwError' instead of the Effectful 'Error' effect.
--
-- Use this module in classic @mtl@ / @transformers@ / @ExceptT@ code.
-- For Effectful prefer the versions from "Railroad".
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

-- | Collapses a structure into its inner type or a `MonadError` error.
collapse :: (MonadError e m, Bifurcate f)
         => (CErr f -> e) -> f -> m (CRes f)
collapse toErr = either (throwError . toErr) pure . bifurcate

-- | Collapses a structure using an error mapper.
(??) :: forall a m e. (MonadError e m, Bifurcate a)
     => m a -> (CErr a -> e) -> m (CRes a)
action ?? toErr = action >>= collapse toErr

-- | Collapses a structure using an error mapper.
(?) :: forall m e a. (MonadError e m, Bifurcate a)
    => m a -> e -> m (CRes a)
action ? err = action ?? const err

-- | Collapses any value based on a predicate
(?>) :: forall m e a. (MonadError e m)
     => m a -> (a -> Bool) -> (a -> e) -> m a
(?>) action predicate toErr = do
  val <- action
  if predicate val then pure val else throwError $ toErr val

-- | Collapses a structure and recovers to a value dependent on the error
(??~) :: forall m a. (Bifurcate a, Monad m) => m a -> (CErr a -> CRes a) -> m (CRes a)
action ??~ defaultFunc = action >>= either (pure . defaultFunc) pure . bifurcate

-- | Collapses a structure, and recovers to a constant default value in the error case
(?~) :: forall m a. (Bifurcate a, Monad m) => m a -> CRes a -> m (CRes a)
action ?~ defaultVal = action ??~ (const defaultVal)

-- |  Non-empty is sucess state. Returns the collection as-is.
(?+) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> e -> m (t a)
(?+) action err = do
  xs <- action
  if null xs then throwError err else pure xs

-- | Single element is sucess state.  Returns the single element
(?!) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (CardinalityError (t a) -> e) -> m a
(?!) action toErr = do
  xs <- action
  case toList xs of
    []  -> throwError $ toErr IsEmpty
    [x] -> pure x
    _   -> throwError $ toErr $ TooMany xs

-- | Empty is success state. Returns Unit.
(?∅) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
(?∅) action toErr = do
  xs <- action
  if null xs then pure () else throwError (toErr xs)




-- | Non-unicode alias for `(?∅)`
(?@) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
(?@) = (?∅)

infixl  0 ??, ?, ??~, ?~, ?!, ?+, ?∅, ?@
infixl 1 ?>
