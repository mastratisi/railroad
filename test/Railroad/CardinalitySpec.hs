module Railroad.CardinalitySpec where

import           Railroad.Cardinality
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "atLeastOne" $ do
    it "classifies a list by emptiness, keeping the list" $ property $ \(xs :: [Int]) ->
      atLeastOne xs === (if null xs then Nothing else Just xs)
    it "works on any Foldable" $ do
      atLeastOne (Just 'a') `shouldBe` Just (Just 'a')
      atLeastOne (Nothing :: Maybe Char) `shouldBe` Nothing

  describe "exactlyOne" $ do
    it "returns the element of a singleton, else says why not" $ property $ \(xs :: [Int]) ->
      exactlyOne xs === (case xs of { [] -> Left IsEmpty; [x] -> Right x; _ -> Left (TooMany xs) })
    it "works on any Foldable" $ do
      exactlyOne (Just 'a') `shouldBe` Right 'a'
      exactlyOne (Nothing :: Maybe Char) `shouldBe` Left IsEmpty

  describe "noneAtAll" $ do
    it "succeeds on an empty collection, else returns it as the failure" $ property $ \(xs :: [Int]) ->
      noneAtAll xs === (if null xs then Right () else Left xs)

  describe "the three agree with each other" $ do
    it "exactlyOne reports IsEmpty exactly when noneAtAll succeeds" $ property $ \(xs :: [Int]) ->
      (exactlyOne xs == Left IsEmpty) === (noneAtAll xs == Right ())
    it "a list of one element satisfies atLeastOne and exactlyOne but not noneAtAll" $ property $ \(x :: Int) ->
      (atLeastOne [x], exactlyOne [x], noneAtAll [x]) === (Just [x], Right x, Left [x])

  describe "CardinalityError" $ do
    it "cardinalityErr is the eliminator" $ property $ \(n :: Int) (xs :: [Int]) ->
      (cardinalityErr n length (IsEmpty :: CardinalityError [Int]) === n)
      .&&. (cardinalityErr n length (TooMany xs) === length xs)
