{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module RailroadSpec where


import           Data.Validation
import           Effectful
import           Effectful.Error.Dynamic
import           Effectful.State.Static.Local
import           Railroad
import           Test.Hspec

-- Helper to run the Railroad effects in a pure context
runRail :: Eff '[Error String] a -> Either String a
runRail = runPureEff . runErrorNoCallStack

-- Like 'runRail', and counts how many 'tick's ran.
runCount :: Eff '[Error String, State Int] a -> (Either String a, Int)
runCount = runPureEff . runState 0 . runErrorNoCallStack

tick :: State Int :> es => Eff es ()
tick = modify (+ (1 :: Int))

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
      let allOk = [Success 1, Success 2] :: [Validation String Int]
      bifurcate allOk `shouldBe` Right [1, 2]
        -- Note: Validation accumulates because String is a Semigroup
      let someBad = [Success 1, Failure "Fail A ", Failure "Fail B"] :: [Validation String Int]
      bifurcate someBad `shouldBe` Left "Fail A Fail B"

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
      runRail (pure (10 :: Int) ?> (> 5) $ const "too small") `shouldBe` Right 10

    it "fails when predicate is not met" $ do
      runRail (pure (4 :: Int) ?> (> 5) $ const "too small") `shouldBe` Left "too small"

  describe "Recovery Operators (?~ and ??~)" $ do
    it "recovers to a constant value with (?~)" $ do
      runPureEff (pure Nothing ?~ 0) `shouldBe` (0 :: Int)

    it "recovers using a function with (??~)" $ do
      runPureEff (pure (Left "err") ??~ length) `shouldBe` 3

  describe "Cardinality Operators" $ do
    describe "(?+)" $ do
      it "succeeds on non-empty list" $ do
        runRail (pure [1, 2, 3 :: Int] ?+ "empty") `shouldBe` Right [1, 2, 3]
      it "fails on empty list" $ do
        runRail (pure ([] :: [Int]) ?+ "empty") `shouldBe` Left "empty"

    describe "(?!)" $ do
      let toErr = cardinalityErr "none" (const "too many")
      it "extracts the single element" $ do
        runRail (pure [42 :: Int] ?! toErr) `shouldBe` Right 42
      it "fails on empty" $ do
        runRail (pure ([] :: [Int]) ?! toErr) `shouldBe` Left "none"
      it "fails on multiple elements" $ do
        runRail (pure [1, 2 :: Int] ?! toErr) `shouldBe` Left "too many"

    describe "(?∅)" $ do
      it "succeeds on empty" $ do
        runRail (pure [] ?∅ const "not empty") `shouldBe` Right ()
      it "fails on non-empty" $ do
        runRail (pure [1 :: Int] ?∅ const "not empty") `shouldBe` Left "not empty"

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
