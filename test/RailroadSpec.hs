{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module RailroadSpec where


import           Data.Validation
import           Effectful
import           Effectful.Error.Dynamic
import           Railroad
import           Test.Hspec

-- Helper to run the Railroad effects in a pure context
runRail :: Eff '[Error String] a -> Either String a
runRail = runPureEff . runErrorNoCallStack

spec :: Spec
spec = do
  describe "Bifurcate Instances" $ do
    it "bifurcates Bool" $ do
      bifurcate True  `shouldBe` (Right () :: Either () ())
      bifurcate False `shouldBe` (Left ()  :: Either () ())

    it "bifurcates Maybe" $ do
      bifurcate (Just 5) `shouldBe` (Right 5 :: Either () Int)
      bifurcate (Nothing :: Maybe Int) `shouldBe` (Left () :: Either () Int)

    it "bifurcates Either" $ do
      bifurcate (Right 5 :: Either String Int) `shouldBe` Right 5
      bifurcate (Left "fire" :: Either String Int) `shouldBe` Left "fire"

    it "bifurcates Validation" $ do
      bifurcate (Success 10 :: Validation String Int) `shouldBe` Right 10
      bifurcate (Failure "ice" :: Validation String Int) `shouldBe` Left "ice"

    it "bifurcates List of Bool" $ do
        bifurcate [True, True] `shouldBe` (Right [(), ()] :: Either () [()])
        bifurcate [True, False, True] `shouldBe` (Left () :: Either () [()])

    it "bifurcates List of Maybe" $ do
      bifurcate [Just 1, Just 2] `shouldBe` (Right [1, 2] :: Either () [Int])
      bifurcate [Just 1, Nothing] `shouldBe` (Left () :: Either () [Int])

    it "bifurcates List of Either" $ do
      let input0 = [Right 1, Right 2] :: [Either String Int]
      bifurcate input0 `shouldBe` Right [1, 2]
      let input1 = [Right 1, Left "first", Left "second"] :: [Either String Int]
      bifurcate input1 `shouldBe` Left "first"

    it "List of Validation (Error Accumulation)" $ do
      let input = [Success 1, Success 2] :: [Validation String Int]
      bifurcate input `shouldBe` Right [1, 2]
        -- Note: Validation accumulates because String is a Semigroup
      let input = [Success 1, Failure "Fail A ", Failure "Fail B"] :: [Validation String Int]
      bifurcate input `shouldBe` Left "Fail A Fail B"

  describe "Basic Operators (? and ??)" $ do
    it "unwraps success values with (?)" $ do
      runRail (pure (Just 10 :: Maybe Int) ? "missing") `shouldBe` Right 10

    it "throws constant error on failure with (?)" $ do
      runRail (pure (Nothing :: Maybe ()) ? "missing") `shouldBe` Left "missing"

    it "maps internal errors with (??)" $ do
      let action = pure (Left "original" :: Either String String)
      runRail (action ?? reverse) `shouldBe` Left "lanigiro"

  describe "Predicate Operator (?>)" $ do
    it "passes when predicate is met" $ do
      runRail (pure 10 ?> (> 5) $ const "too small") `shouldBe` Right 10

    it "fails when predicate is not met" $ do
      runRail (pure 4 ?> (> 5) $ const "too small") `shouldBe` Left "too small"

  describe "Recovery Operators (?~ and ??~)" $ do
    it "recovers to a constant value with (?~)" $ do
      runPureEff (pure Nothing ?~ 0) `shouldBe` (0 :: Int)

    it "recovers using a function with (??~)" $ do
      runPureEff (pure (Left "err") ??~ length) `shouldBe` 3

  describe "Cardinality Operators" $ do
    describe "(?+)" $ do
      it "succeeds on non-empty list" $ do
        runRail (pure [1, 2, 3] ?+ "empty") `shouldBe` Right [1, 2, 3]
      it "fails on empty list" $ do
        runRail (pure ([] :: [Int]) ?+ "empty") `shouldBe` Left "empty"

    describe "(?!)" $ do
      let toErr = cardinalityErr "none" (const "too many")
      it "extracts the single element" $ do
        runRail (pure [42] ?! toErr) `shouldBe` Right 42
      it "fails on empty" $ do
        runRail (pure ([] :: [Int]) ?! toErr) `shouldBe` Left "none"
      it "fails on multiple elements" $ do
        runRail (pure [1, 2] ?! toErr) `shouldBe` Left "too many"

    describe "(?∅)" $ do
      it "succeeds on empty" $ do
        runRail (pure [] ?∅ const "not empty") `shouldBe` Right ()
      it "fails on non-empty" $ do
        runRail (pure [1] ?∅ const "not empty") `shouldBe` Left "not empty"
