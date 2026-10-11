{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Railroad.MonadErrorSpec where

import           Control.Monad.Except
import           Control.Monad.State   (State, modify, runState)
import           Data.Validation       (Validation (..))
import           Data.Functor.Identity
import           Railroad.MonadError
import           Test.Hspec

-- Helper to run MonadError computations in pure Either
runMonadError :: ExceptT String Identity a -> Either String a
runMonadError = runIdentity . runExceptT

-- Like 'runMonadError', and counts how many 'tick's ran.
runCount :: ExceptT String (State Int) a -> (Either String a, Int)
runCount m = runState (runExceptT m) 0

tick :: ExceptT String (State Int) ()
tick = modify (+ 1)

spec :: Spec
spec = do
  describe "MonadError version of Operators" $ do
    describe "Basic Operators (? and ??)" $ do
      it "unwraps success values with (?)" $ do
        runMonadError (pure (Just 10 :: Maybe Int) ? "missing") `shouldBe` Right 10
      it "throws constant error on failure with (?)" $ do
        runMonadError (pure (Nothing :: Maybe ()) ? "missing") `shouldBe` Left "missing"
      it "maps internal errors with (??)" $ do
        let action = pure (Left "original" :: Either String String)
        runMonadError (action ?? reverse) `shouldBe` Left "lanigiro"

    describe "Predicate Operator (?>)" $ do
      it "passes when predicate is met" $ do
        runMonadError (pure (10 :: Int) ?> (> 5) $ const "too small") `shouldBe` Right 10
      it "fails when predicate is not met" $ do
        runMonadError (pure (4 :: Int) ?> (> 5) $ const "too small") `shouldBe` Left "too small"

    describe "Recovery Operators (?~ and ??~)" $ do
      it "recovers to a constant value with (?~)" $ do
        runMonadError (pure Nothing ?~ 0) `shouldBe` Right (0 :: Int)
      it "recovers using a function with (??~)" $ do
        runMonadError (pure (Left "err") ??~ length) `shouldBe` Right 3

    describe "Cardinality Operators" $ do
      describe "(?+)" $ do
        it "succeeds on non-empty list" $ do
          runMonadError (pure [1, 2, 3 :: Int] ?+ "empty") `shouldBe` Right [1, 2, 3]
        it "fails on empty list" $ do
          runMonadError (pure ([] :: [Int]) ?+ "empty") `shouldBe` Left "empty"

      describe "(?!)" $ do
        let toErr = cardinalityErr "none" (const "too many")
        it "extracts the single element" $ do
          runMonadError (pure [42 :: Int] ?! toErr) `shouldBe` Right 42
        it "fails on empty" $ do
          runMonadError (pure ([] :: [Int]) ?! toErr) `shouldBe` Left "none"
        it "fails on multiple elements" $ do
          runMonadError (pure [1, 2 :: Int] ?! toErr) `shouldBe` Left "too many"

      describe "(?∅)" $ do
        it "succeeds on empty" $ do
          runMonadError (pure [] ?∅ const "not empty") `shouldBe` Right ()
        it "fails on non-empty" $ do
          runMonadError (pure [1 :: Int] ?∅ const "not empty") `shouldBe` Left "not empty"

    describe "Fallback Operator (?|)" $ do
      it "does not run the right action when the left succeeds" $ do
        runCount (pure (Just 'a') ?| (tick >> pure (Nothing :: Maybe Char)) ? "none")
          `shouldBe` (Right 'a', 0)
      it "runs the right action when the left fails" $ do
        runCount (pure (Nothing :: Maybe Int) ?| (tick >> pure (Just 7)) ? "none")
          `shouldBe` (Right 7, 1)
      it "keeps the last error when everything fails" $ do
        runCount (pure (Left "first" :: Either String Int) ?| pure (Left "second") ?? id)
          `shouldBe` (Left "second", 0)
        runCount (pure (Nothing :: Maybe Int) ?| pure Nothing ? "none")
          `shouldBe` (Left "none", 0)
      it "chains mixed structures, stopping at the first success" $ do
        let chain = pure (Nothing :: Maybe Int) ?| pure (Left "db" :: Either String Int)
                      ?| (tick >> pure (Just 3)) ?| (tick >> tick >> pure (Just 4)) ? "gone"
        runCount chain `shouldBe` (Right 3, 1)

    describe "Accumulating Fallback Operator (?|<>)" $ do
      it "does not run the right action when the left succeeds" $ do
        runCount (pure (Just 'a') ?|<> (tick >> pure (Nothing :: Maybe Char)) ? "none")
          `shouldBe` (Right 'a', 0)
      it "combines the errors in source order when everything fails" $ do
        runCount (pure (Left "A" :: Either String Int) ?|<> pure (Left "B") ?|<> pure (Left "C") ?? id)
          `shouldBe` (Left "ABC", 0)
      it "mixes structures with the same error and success types" $ do
        runCount (pure (Left "A" :: Either String Int) ?|<> pure (Failure "B" :: Validation String Int) ?? id)
          `shouldBe` (Left "AB", 0)
        runCount (pure (Left "A" :: Either String Int) ?|<> (tick >> pure (Success 5 :: Validation String Int)) ?? id)
          `shouldBe` (Right 5, 1)
      it "drops earlier errors once a later source succeeds" $ do
        runCount (pure (Left "A" :: Either String Int) ?|<> pure (Left "B") ?|<> pure (Right 3) ?? id)
          `shouldBe` (Right 3, 0)
      it "needs nothing special for Maybe sources" $ do
        runCount (pure (Nothing :: Maybe Int) ?|<> pure Nothing ? "none")
          `shouldBe` (Left "none", 0)

    describe "Layered structures (the outermost layer is peeled first)" $ do
      it "peels Either of Maybe one layer at a time" $ do
        runMonadError (pure (Right (Just 1) :: Either String (Maybe Int)) ? "outer" ? "inner") `shouldBe` Right 1
        runMonadError (pure (Right Nothing :: Either String (Maybe Int)) ? "outer" ? "inner") `shouldBe` Left "inner"
        runMonadError (pure (Left "x" :: Either String (Maybe Int)) ? "outer" ? "inner") `shouldBe` Left "outer"
      it "peels Maybe of Maybe and Either of Bool" $ do
        runMonadError (pure (Just Nothing :: Maybe (Maybe Int)) ? "o" ? "i") `shouldBe` Left "i"
        runMonadError (pure (Right False :: Either String Bool) ? "db" ? "denied") `shouldBe` Left "denied"
      it "runs the README example" $ do
        let readmeExample :: Either String Int
            readmeExample = runExcept $ do
              x <- pure (Just 2) ? "Value missing"
              y <- pure (Right $ Just 1) ? "Outer fail" ? "Inner fail"
              z <- pure [Just 4] ? "List failed" ?! const "Not a single element"
              q <- pure Nothing ?~ 1
              pure (x + y + z + q)
        readmeExample `shouldBe` Right 8
