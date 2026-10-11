{-# LANGUAGE LambdaCase #-}

-- |
-- The error info of the cardinality operators (@?!@ in "Railroad" and "Railroad.MonadError"):
-- why a collection was not a single element.
module Railroad.Cardinality
  ( CardinalityError (..), cardinalityErr
  ) where

-- | Why a collection was not a single element: empty, or too many (carrying it).
data CardinalityError ta = IsEmpty | TooMany ta

-- | Fold for 'CardinalityError': one result per case.
cardinalityErr :: e -> (ta -> e) -> CardinalityError ta -> e
cardinalityErr onEmpty onTooMany = \case
  IsEmpty -> onEmpty
  TooMany xs -> onTooMany xs
