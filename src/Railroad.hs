{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts     #-}

-- |
-- Railway-oriented operators: turn a failure in a 'Bifurcate' structure (@Bool@, @Maybe@,
-- @Either@, @Validation@, traversables of them) or a bad cardinality into a throw in the
-- @Error@ effect, keeping the happy path clean. For @MonadError@ use "Railroad.MonadError".
module Railroad
  ( module Railroad.Bifurcate
  , module Railroad.Cardinality
  , collapse
  , (??), (?), (?+), (?!), (?∅), (?@)
  ) where

import           Data.Foldable           (toList)
import           Effectful               (Eff, type (:>))
import           Effectful.Error.Dynamic (Error, throwError_)
import           Railroad.Bifurcate
import           Railroad.Cardinality


-- | Unwraps the success case, or throws the failure mapped by the function.
collapse :: (Error e :> es, Bifurcate f) => (CErr f -> e) -> f -> Eff es (CRes f)
collapse toErr = either (throwError_ . toErr) pure . bifurcate


-- | Unwraps the success case, or throws the error info mapped by the function.
(??) :: forall a es e. (Error e :> es, Bifurcate a) => Eff es a -> (CErr a -> e) -> Eff es (CRes a)
action ?? toErr = action >>= collapse toErr

-- | Unwraps the success case, or throws a constant error. Chains to unwrap nested layers:
-- @m ? e1 ? e2@.
(?) :: forall es e a. (Error e :> es, Bifurcate a) => Eff es a -> e -> Eff es (CRes a)
action ? err = action ?? const err


-- | Succeeds if non-empty, returning the collection; else throws the constant error.
-- Chain it before @?@ on a traversable, which an empty collection passes.
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

infixl 1 ??, ?, ?!, ?+, ?∅, ?@
