{-# LANGUAGE LambdaCase #-}

-- |
-- The error info of the cardinality operators (@?!@ in "Railroad" and "Railroad.MonadError"):
-- why a collection was not a single element. Also the classifiers those operators are
-- built from, which are pure and need no monad.
module Railroad.Cardinality
  ( CardinalityError (..), cardinalityErr
  , atLeastOne, exactlyOne, noneAtAll
  ) where

import           Data.Foldable (toList)

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
