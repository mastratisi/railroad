{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Railroad.MonadErrorSpec where

import           Control.Monad.Except
import           Data.Functor.Identity
import           Data.Validation
import           Railroad.MonadError
import           Test.Hspec

-- Helper to run MonadError computations in pure Either
runMonadError :: ExceptT String Identity a -> Either String a
runMonadError = runIdentity . runExceptT

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
        runMonadError (pure 10 ?> (> 5) $ const "too small") `shouldBe` Right 10
      it "fails when predicate is not met" $ do
        runMonadError (pure 4 ?> (> 5) $ const "too small") `shouldBe` Left "too small"

    describe "Recovery Operators (?~ and ??~)" $ do
      it "recovers to a constant value with (?~)" $ do
        runMonadError (pure Nothing ?~ 0) `shouldBe` Right (0 :: Int)
      it "recovers using a function with (??~)" $ do
        runMonadError (pure (Left "err") ??~ length) `shouldBe` Right 3

    describe "Cardinality Operators" $ do
      describe "(?+)" $ do
        it "succeeds on non-empty list" $ do
          runMonadError (pure [1, 2, 3] ?+ "empty") `shouldBe` Right [1, 2, 3]
        it "fails on empty list" $ do
          runMonadError (pure ([] :: [Int]) ?+ "empty") `shouldBe` Left "empty"

      describe "(?!)" $ do
        let toErr = cardinalityErr "none" (const "too many")
        it "extracts the single element" $ do
          runMonadError (pure [42] ?! toErr) `shouldBe` Right 42
        it "fails on empty" $ do
          runMonadError (pure ([] :: [Int]) ?! toErr) `shouldBe` Left "none"
        it "fails on multiple elements" $ do
          runMonadError (pure [1, 2] ?! toErr) `shouldBe` Left "too many"

      describe "(?∅)" $ do
        it "succeeds on empty" $ do
          runMonadError (pure [] ?∅ const "not empty") `shouldBe` Right ()
        it "fails on non-empty" $ do
          runMonadError (pure [1] ?∅ const "not empty") `shouldBe` Left "not empty"
