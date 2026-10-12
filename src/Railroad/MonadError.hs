-- |
-- The operators of "Railroad" for any 'MonadError' (@mtl@, @transformers@, @ExceptT@,
-- servant's @Handler@) instead of the @Error@ effect. In an @effectful@ stack use "Railroad".
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Railroad.MonadError
  ( module Railroad.Bifurcate
  , module Railroad.Cardinality
  , collapse
  , (??), (?), (?+), (?!), (?∅), (?@), (?∃)
  ) where

import           Control.Monad.Except (MonadError (..))
import           Data.Functor         ((<&>))
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

-- | Unwraps the success case, or throws a constant error. Chains to unwrap nested layers:
-- @m ? e1 ? e2@.
(?) :: forall m e a. (MonadError e m, Bifurcate a)
    => m a -> e -> m (CRes a)
action ? err = action ?? const err

-- | Succeeds if non-empty, returning the collection; else throws the constant error.
-- Chain it before @?@ on a traversable, which an empty collection passes.
-- Sugar for @xs <&> 'atLeastOne' ? err@.
(?+) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> e -> m (t a)
action ?+ err = action <&> atLeastOne ? err

-- | Succeeds if there is exactly one element, returning it; else throws the error built
-- from the 'CardinalityError'. Sugar for @xs <&> 'exactlyOne' ?? toErr@.
(?!) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (CardinalityError (t a) -> e) -> m a
action ?! toErr = action <&> exactlyOne ?? toErr

-- | Succeeds if empty, returning @()@; else throws the error built from the collection.
-- Sugar for @xs <&> 'noneAtAll' ?? toErr@.
(?∅) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
action ?∅ toErr = action <&> noneAtAll ?? toErr

-- | ASCII alias for '?∅'.
(?@) :: forall m e t a. (MonadError e m, Foldable t)
     => m (t a) -> (t a -> e) -> m ()
(?@) = (?∅)

-- | Succeeds if at least one element succeeds, returning the successes; else throws the
-- error built from every failure (none if the collection was empty). Unlike '?+' it asks
-- whether an element succeeded, not whether there is one. On a nested base type such as
-- @Maybe (Maybe a)@ the outer layer is the collection and the inner one is checked.
-- Sugar for @xs <&> 'anySucceeds' ?? toErr@.
(?∃) :: forall m e t a. (MonadError e m, Foldable t, Bifurcate a)
     => m (t a) -> ([CErr a] -> e) -> m [CRes a]
action ?∃ toErr = action <&> anySucceeds ?? toErr

infixl 1 ??, ?, ?!, ?+, ?∅, ?@, ?∃
