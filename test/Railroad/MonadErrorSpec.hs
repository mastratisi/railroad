{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Railroad.MonadErrorSpec where

import           Control.Monad.Except
import           Control.Monad.State   (modify, runState)
import           Data.Bifunctor        (first)
import           Data.Either           (isRight)
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

  describe "an empty collection passes ? (vacuous truth); ?+ requires an element" $ do
    it "[] passes ? on [Bool]" $ do
      runMonadError (pure ([] :: [Bool]) ? "denied") `shouldBe` Right []
    it "?+ then ? needs at least one element, all succeeding" $ property $ \(bs :: [Bool]) ->
      runMonadError (pure bs ?+ "none" ? "denied")
        === (if null bs then Left "none" else if and bs then Right (map (const ()) bs) else Left "denied")

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
    it "logs a failure with ?<*, then still throws the mapped error" $ do
      let run m = runState (runExceptT m) ([] :: [Int])
      run (pure (Left 3 :: Either Int Int) ?<* (\e -> modify (e :)) ? "failed") `shouldBe` (Left "failed", [3])
      run (pure (Right 4 :: Either Int Int) ?<* (\e -> modify (e :)) ? "failed") `shouldBe` (Right 4, [])
    it "gives the rejected value of ?> to the error mapper" $ do
      runMonadError (pure (4 :: Int) ?> (> 5) ?? \n -> "rejected " ++ show n) `shouldBe` Left "rejected 4"

  describe "?∃ (at least one success)" $ do
    it "returns the successes, or throws every failure" $ property $ \(xs :: [Either Int Int]) ->
      runMonadError (pure xs ?∃ show)
        === (if any isRight xs then Right [a | Right a <- xs] else Left (show [e | Left e <- xs]))
    it "throws on an empty collection, with no failures" $
      runMonadError (pure ([] :: [Maybe Int]) ?∃ show) `shouldBe` Left "[]"
    it "passes the successes of a Maybe collection and throws unit failures" $ do
      runMonadError (pure [Just 1, Nothing, Just (3 :: Int)] ?∃ show) `shouldBe` Right [1, 3]
      runMonadError (pure [Nothing, Nothing :: Maybe Int] ?∃ show) `shouldBe` Left "[(),()]"
    it "throws where ?+ passes: every element failed" $ do
      runMonadError (pure [Left 1, Left 2 :: Either Int Int] ?+ "none") `shouldSatisfy` isRight
      runMonadError (pure [Left 1, Left 2 :: Either Int Int] ?∃ show) `shouldBe` Left "[1,2]"
    it "chains left to right with the other operators" $ do
      runMonadError (pure (Right [Just 1, Nothing] :: Either String [Maybe Int]) ? "db" ?∃ show) `shouldBe` Right [1]

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
