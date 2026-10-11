{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module RailroadSpec where

import           Data.Bifunctor          (first)
import           Data.Functor            ((<&>))
import           Data.Proxy              (Proxy (..))
import           Effectful
import           Effectful.Error.Dynamic
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
    it "gives the rejected value of ?> to the error mapper" $ do
      runRail (pure (4 :: Int) ?> (> 5) ?? \n -> "rejected " ++ show n) `shouldBe` Left "rejected 4"
