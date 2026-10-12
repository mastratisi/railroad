{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module RailroadSpec where

import           Data.Bifunctor          (first)
import           Data.Either             (isRight)
import           Data.Functor            ((<&>))
import           Data.Proxy              (Proxy (..))
import           Effectful
import           Effectful.Error.Dynamic
import           Effectful.State.Static.Local (State, modify, runState)
import           Model
import           Railroad
import           Test.Hspec
import           Test.QuickCheck

-- Helper to run the Railroad effects in a pure context
runRail :: Eff '[Error String] a -> Either String a
runRail = runPureEff . runErrorNoCallStack

spec :: Spec
spec = do
  forShapes "collapse, ?? and ? throw the mapped error info, else unwrap" $ \(_ :: Proxy a) ->
    property $ \(x :: a) ->
      (runRail (collapse show x) === first show (bifurcate x))
      .&&. (runRail (pure x ?? show) === first show (bifurcate x))
      .&&. (runRail (pure x ? "boom") === first (const "boom") (bifurcate x))

  describe "cardinality operators" $ do
    it "?+ succeeds on a non-empty collection" $ property $ \(xs :: [Int]) ->
      runRail (pure xs ?+ "empty") === (if null xs then Left "empty" else Right xs)
    it "?! succeeds on exactly one element" $ property $ \(xs :: [Int]) ->
      runRail (pure xs ?! cardinalityErr "none" (\ys -> "many " ++ show (length ys)))
        === (case xs of { [] -> Left "none"; [x] -> Right x; _ -> Left ("many " ++ show (length xs)) })
    it "?∅ and ?@ succeed on an empty collection" $ property $ \(xs :: [Int]) ->
      let model = if null xs then Right () else Left ("got " ++ show (length xs))
      in (runRail (pure xs ?∅ (\ys -> "got " ++ show (length ys))) === model)
         .&&. (runRail (pure xs ?@ (\ys -> "got " ++ show (length ys))) === model)

  describe "an empty collection passes ? (vacuous truth); ?+ requires an element" $ do
    it "[] passes ? on [Bool]" $ do
      runRail (pure ([] :: [Bool]) ? "denied") `shouldBe` Right []
    it "?+ then ? needs at least one element, all succeeding" $ property $ \(bs :: [Bool]) ->
      runRail (pure bs ?+ "none" ? "denied")
        === (if null bs then Left "none" else if and bs then Right (map (const ()) bs) else Left "denied")

  describe "fixity (all operators are infixl 1, like >>=, <&> and &)" $ do
    it "composes with <&> without parentheses" $ do
      runRail (pure (Right 2 :: Either String Int) ? "e" <&> (+ 1)) `shouldBe` Right 3
    it "composes with $" $ do
      runRail (fmap negate $ pure (Just 2 :: Maybe Int) ? "e") `shouldBe` Right (-2)
    it "chains ? and ?> left to right" $ do
      runRail (pure (Just 7 :: Maybe Int) ? "none" ?> (> 5) ? "small") `shouldBe` Right 7
      runRail (pure (Just 3 :: Maybe Int) ? "none" ?> (> 5) ? "small") `shouldBe` Left "small"
    it "chains the fallback with ? and ?>" $ do
      runRail (pure (Nothing :: Maybe Int) ?| pure (Right 9 :: Either String Int) ? "none" ?> (> 5) ? "small")
        `shouldBe` Right 9
    it "logs a failure with ?<*, then still throws the mapped error" $ do
      let run :: Eff '[Error String, State [Int]] a -> (Either String a, [Int])
          run = runPureEff . runState [] . runErrorNoCallStack
      run (pure (Left 3 :: Either Int Int) ?<* (\e -> modify (e :)) ? "failed") `shouldBe` (Left "failed", [3])
      run (pure (Right 4 :: Either Int Int) ?<* (\e -> modify (e :)) ? "failed") `shouldBe` (Right 4, [])
    it "gives the rejected value of ?> to the error mapper" $ do
      runRail (pure (4 :: Int) ?> (> 5) ?? \n -> "rejected " ++ show n) `shouldBe` Left "rejected 4"

  describe "?∃ (at least one success)" $ do
    it "returns the successes, or throws every failure" $ property $ \(xs :: [Either Int Int]) ->
      runRail (pure xs ?∃ show)
        === (if any isRight xs then Right [a | Right a <- xs] else Left (show [e | Left e <- xs]))
    it "throws on an empty collection, with no failures" $
      runRail (pure ([] :: [Maybe Int]) ?∃ show) `shouldBe` Left "[]"
    it "passes the successes of a Maybe collection and throws unit failures" $ do
      runRail (pure [Just 1, Nothing, Just (3 :: Int)] ?∃ show) `shouldBe` Right [1, 3]
      runRail (pure [Nothing, Nothing :: Maybe Int] ?∃ show) `shouldBe` Left "[(),()]"
    it "throws where ?+ passes: every element failed" $ do
      runRail (pure [Left 1, Left 2 :: Either Int Int] ?+ "none") `shouldSatisfy` isRight
      runRail (pure [Left 1, Left 2 :: Either Int Int] ?∃ show) `shouldBe` Left "[1,2]"
    it "chains left to right with the other operators" $ do
      runRail (pure (Right [Just 1, Nothing] :: Either String [Maybe Int]) ? "db" ?∃ show) `shouldBe` Right [1]
