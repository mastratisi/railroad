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
  , (??), (?), (?+), (?!), (?∅), (?@), (?∃)
  ) where

import           Data.Functor            ((<&>))
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
-- Sugar for @xs <&> 'atLeastOne' ? err@.
(?+) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> e -> Eff es (t a)
action ?+ err = action <&> atLeastOne ? err


-- | Succeeds if there is exactly one element, returning it; else throws the error built
-- from the 'CardinalityError'. Sugar for @xs <&> 'exactlyOne' ?? toErr@.
(?!) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> (CardinalityError (t a) -> e) -> Eff es a
action ?! toErr = action <&> exactlyOne ?? toErr


-- | Succeeds if empty, returning @()@; else throws the error built from the collection.
-- Sugar for @xs <&> 'noneAtAll' ?? toErr@.
(?∅) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
action ?∅ toErr = action <&> noneAtAll ?? toErr

-- | ASCII alias for '?∅'.
(?@) :: forall es e t a. (Error e :> es, Foldable t)
     => Eff es (t a) -> ((t a) -> e) -> Eff es ()
(?@) = (?∅)

-- | Succeeds if at least one element succeeds, returning the successes; else throws the
-- error built from every failure (none if the collection was empty). Unlike '?+' it asks
-- whether an element succeeded, not whether there is one. On a nested base type such as
-- @Maybe (Maybe a)@ the outer layer is the collection and the inner one is checked.
-- Sugar for @xs <&> 'anySucceeds' ?? toErr@.
(?∃) :: forall es e t a. (Error e :> es, Foldable t, Bifurcate a)
     => Eff es (t a) -> ([CErr a] -> e) -> Eff es [CRes a]
action ?∃ toErr = action <&> anySucceeds ?? toErr

infixl 1 ??, ?, ?!, ?+, ?∅, ?@, ?∃
