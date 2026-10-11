{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Railroad.MonadErrorSpec where

import           Control.Monad.Except
import           Data.Bifunctor        (first)
import           Data.Functor          ((<&>))
import           Data.Functor.Identity
import           Data.Proxy            (Proxy (..))
import           Model
import           Railroad.MonadError
import           Test.Hspec
import           Test.QuickCheck

-- Helper to run MonadError computations in pure Either
runMonadError :: ExceptT String Identity a -> Either String a
runMonadError = runIdentity . runExceptT

spec :: Spec
spec = do
  forShapes "collapse, ?? and ? throw the mapped error info, else unwrap" $ \(_ :: Proxy a) ->
    property $ \(x :: a) ->
      (runMonadError (collapse show x) === first show (bifurcate x))
      .&&. (runMonadError (pure x ?? show) === first show (bifurcate x))
      .&&. (runMonadError (pure x ? "boom") === first (const "boom") (bifurcate x))

  describe "cardinality operators" $ do
    it "?+ succeeds on a non-empty collection" $ property $ \(xs :: [Int]) ->
      runMonadError (pure xs ?+ "empty") === (if null xs then Left "empty" else Right xs)
    it "?! succeeds on exactly one element" $ property $ \(xs :: [Int]) ->
      runMonadError (pure xs ?! cardinalityErr "none" (\ys -> "many " ++ show (length ys)))
        === (case xs of { [] -> Left "none"; [x] -> Right x; _ -> Left ("many " ++ show (length xs)) })
    it "?∅ and ?@ succeed on an empty collection" $ property $ \(xs :: [Int]) ->
      let model = if null xs then Right () else Left ("got " ++ show (length xs))
      in (runMonadError (pure xs ?∅ (\ys -> "got " ++ show (length ys))) === model)
         .&&. (runMonadError (pure xs ?@ (\ys -> "got " ++ show (length ys))) === model)

  describe "fixity (all operators are infixl 1, like >>=, <&> and &)" $ do
    it "composes with <&> without parentheses" $ do
      runMonadError (pure (Right 2 :: Either String Int) ? "e" <&> (+ 1)) `shouldBe` Right 3
    it "composes with $" $ do
      runMonadError (fmap negate $ pure (Just 2 :: Maybe Int) ? "e") `shouldBe` Right (-2)
    it "chains ? and ?> left to right" $ do
      runMonadError (pure (Just 7 :: Maybe Int) ? "none" ?> (> 5) ? "small") `shouldBe` Right 7
      runMonadError (pure (Just 3 :: Maybe Int) ? "none" ?> (> 5) ? "small") `shouldBe` Left "small"
    it "chains the fallback with ? and ?>" $ do
      runMonadError (pure (Nothing :: Maybe Int) ?| pure (Right 9 :: Either String Int) ? "none" ?> (> 5) ? "small")
        `shouldBe` Right 9
    it "gives the rejected value of ?> to the error mapper" $ do
      runMonadError (pure (4 :: Int) ?> (> 5) ?? \n -> "rejected " ++ show n) `shouldBe` Left "rejected 4"

  describe "README" $ do
    it "runs the first example" $ do
      let readmeExample :: Either String Int
          readmeExample = runExcept $ do
            x <- pure (Just 2) ? "Value missing"
            y <- pure (Right $ Just 1) ? "Outer fail" ? "Inner fail"
            z <- pure [Just 4] ? "List failed" ?! const "Not a single element"
            q <- pure Nothing ?~ 1
            pure (x + y + z + q)
      readmeExample `shouldBe` Right 8
