{-# LANGUAGE TypeFamilies #-}

module Railroad.BifurcateSpec where

import           Control.Monad.State    (modify, runState)
import           Data.Bifunctor         (first)
import           Data.Either            (isLeft, isRight)
import           Data.Functor.Identity
import           Data.Maybe             (listToMaybe)
import           Data.Proxy             (Proxy (..))
import           Data.Validation        (Validation (..))
import           Model
import           Railroad.Bifurcate
import           Railroad.Cardinality   (atLeastOne, noneAtAll)
import           Test.Hspec
import           Test.QuickCheck        hiding (Result (..))

spec :: Spec
spec = do
  describe "bifurcate" $ do
    it "Bool" $ property $ \b ->
      bifurcate b === (if b then Right () else Left ())
    it "Maybe" $ property $ \(m :: Maybe Int) ->
      bifurcate m === maybe (Left ()) Right m
    it "Either is the identity" $ property $ \(e :: Either Int Int) ->
      bifurcate e === e
    it "Validation" $ property $ \(v :: Validation Int Int) ->
      bifurcate v === (case v of Failure e -> Left e; Success a -> Right a)
    it "a traversable of Bool or Maybe succeeds only if every element does" $ property $
      \(bs :: [Bool]) (ms :: [Maybe Int]) ->
        (bifurcate bs === (if and bs then Right (map (const ()) bs) else Left ()))
        .&&. (bifurcate ms === maybe (Left ()) Right (sequence ms))
    it "a traversable of Either stops at the first error" $ property $ \(xs :: [Either Int Int]) ->
      bifurcate xs === maybe (Right [a | Right a <- xs]) Left (listToMaybe [e | Left e <- xs])
    it "a traversable of Validation accumulates every error" $ property $ \(xs :: [Validation [Int] Int]) ->
      bifurcate xs === (case [e | Failure e <- xs] of
                          [] -> Right [a | Success a <- xs]
                          es -> Left (concat es))
    it "unwraps only the outermost base layer" $ property $
      \(e :: Either Int (Maybe Int)) (m :: Maybe (Maybe Int)) (b :: Either Int Bool) ->
        (bifurcate e === e) .&&. (bifurcate m === maybe (Left ()) Right m) .&&. (bifurcate b === b)

  describe "?>" $ do
    it "tags by the predicate, keeping the value on both sides" $ property $ \(a :: Int) ->
      runIdentity (pure a ?> even) === (if even a then Right a else Left a)

  forShapes "?~ and ??~ recover from a failure" $ \(_ :: Proxy a) -> property $
    \(x :: a) (d :: CRes a) (Fn f :: Fun (CErr a) (CRes a)) ->
      (runIdentity (pure x ?~ d) === either (const d) id (bifurcate x))
      .&&. (runIdentity (pure x ??~ f) === either f id (bifurcate x))

  forShapes "?<* runs the handler on a failure only and passes the value through" $ \(_ :: Proxy a) -> property $
    \(x :: a) ->
      let (y, logged) = runState (pure x ?<* (\e -> modify (e :))) ([] :: [CErr a])
      in (bifurcate y === bifurcate x) .&&. (logged === either pure (const []) (bifurcate x))

  describe "?| (fallback, last error kept)" $ do
    let law :: forall a b. (Shape a, Shape b, CRes a ~ CRes b) => Proxy a -> Proxy b -> Property
        law _ _ = property $ \(x :: a) (y :: b) ->
          runIdentity (pure x ?| pure y) === either (const (bifurcate y)) Right (bifurcate x)
        shortCircuits :: forall a b. (Shape a, Shape b, CRes a ~ CRes b) => Proxy a -> Proxy b -> Property
        shortCircuits _ _ = property $ \(x :: a) (y :: b) ->
          snd (runState (pure x ?| (modify (+ (1 :: Int)) >> pure y)) 0)
            === (if isLeft (bifurcate x) then 1 else 0)
    it "Maybe, Either"      $ law (Proxy @(Maybe Int)) (Proxy @(Either Int Int))
    it "Either, Maybe"      $ law (Proxy @(Either Int Int)) (Proxy @(Maybe Int))
    it "Validation, Either" $ law (Proxy @(Validation Int Int)) (Proxy @(Either Int Int))
    it "[Maybe], [Either]"  $ law (Proxy @[Maybe Int]) (Proxy @[Either Int Int])
    it "runs the right action only when the left fails" $ shortCircuits (Proxy @(Either Int Int)) (Proxy @(Maybe Int))
    it "is associative" $ property $ \(a :: Either Int Int) (b :: Either Int Int) (c :: Either Int Int) ->
      runIdentity ((pure a ?| pure b) ?| pure c) === runIdentity (pure a ?| (pure b ?| pure c))

  describe "?|<> (fallback, errors combined with <>)" $ do
    let law :: forall a b. (Shape a, Shape b, CRes a ~ CRes b, CErr a ~ CErr b, Semigroup (CErr a))
            => Proxy a -> Proxy b -> Property
        law _ _ = property $ \(x :: a) (y :: b) ->
          runIdentity (pure x ?|<> pure y)
            === either (\ex -> first (ex <>) (bifurcate y)) Right (bifurcate x)
    it "Either, Validation" $ law (Proxy @(Either [Int] Int)) (Proxy @(Validation [Int] Int))
    it "Validation, Either" $ law (Proxy @(Validation [Int] Int)) (Proxy @(Either [Int] Int))
    it "Maybe, Maybe"       $ law (Proxy @(Maybe Int)) (Proxy @(Maybe Int))
    it "[Either], [Either]" $ law (Proxy @[Either [Int] Int]) (Proxy @[Either [Int] Int])
    it "is associative" $ property $ \(a :: Either [Int] Int) (b :: Either [Int] Int) (c :: Either [Int] Int) ->
      runIdentity ((pure a ?|<> pure b) ?|<> pure c) === runIdentity (pure a ?|<> (pure b ?|<> pure c))

  describe "tagBy" $ do
    it "keeps the value on both sides" $ property $ \(p :: Fun Int Bool) (x :: Int) ->
      either id id (tagBy (applyFun p) x) === x
    it "is Right exactly when the predicate holds" $ property $ \(p :: Fun Int Bool) (x :: Int) ->
      isRight (tagBy (applyFun p) x) === applyFun p x

  describe "partitionBifurcate, successes, failures" $ do
    let laws :: forall a. (Shape a, Bifurcate [a], CErr [a] ~ CErr a, CRes [a] ~ [CRes a])
             => Proxy a -> Property
        laws _ = property $ \(xs :: [a]) (ys :: [a]) ->
          let (es, rs) = partitionBifurcate xs
          in (length es + length rs === length xs)
             .&&. (partitionBifurcate (xs ++ ys) === (es <> failures ys, rs <> successes ys))
             .&&. (isRight (bifurcate xs) === null es)
             .&&. (either (const True) (== rs) (bifurcate xs))
             .&&. (successes xs === rs)
             .&&. (failures xs === es)
        single :: forall a. Shape a => Proxy a -> Property
        single _ = property $ \(x :: a) ->
          (successes [x] === either (const []) pure (bifurcate x))
          .&&. (failures [x] === either pure (const []) (bifurcate x))
    it "Bool"       $ laws (Proxy @Bool)
    it "Maybe"      $ laws (Proxy @(Maybe Int))
    it "Either"     $ laws (Proxy @(Either Int Int))
    it "Validation" $ laws (Proxy @(Validation [Int] Int))
    forShapes "a singleton splits as its element does" single

  describe "anySucceeds" $ do
    let law :: forall a. (Shape a) => Proxy a -> Property
        law _ = property $ \(xs :: [a]) ->
          let r = anySucceeds xs
          in (isRight r === any (isRight . bifurcate) xs)
             .&&. (r === (if any (isRight . bifurcate) xs then Right (successes xs) else Left (failures xs)))
             .&&. (either length (const 0) r === (if any (isRight . bifurcate) xs then 0 else length xs))
    it "Bool"       $ law (Proxy @Bool)
    it "Maybe"      $ law (Proxy @(Maybe Int))
    it "Either"     $ law (Proxy @(Either Int Int))
    it "Validation" $ law (Proxy @(Validation [Int] Int))
    it "fails on an empty collection with no failures" $
      anySucceeds ([] :: [Maybe Int]) `shouldBe` Left []

  describe "neg" $ do
    it "Bool and Maybe swap to the unit failure" $ do
      neg True `shouldBe` Left ()
      neg False `shouldBe` Right ()
      neg (Just 'x') `shouldBe` Left 'x'
      neg (Nothing :: Maybe Char) `shouldBe` Right ()
    forShapes "swaps failure and success" $ \(_ :: Proxy a) -> property $
      \(x :: a) -> neg x === either Right Left (bifurcate x)
    forShapes "is an involution: neg (neg x) = bifurcate x" $ \(_ :: Proxy a) -> property $
      \(x :: a) -> neg (neg x) === bifurcate x
    it "of a traversable passes exactly when some element failed" $ property $
      \(xs :: [Either Int Int]) -> isRight (neg xs) === any (isLeft . bifurcate) xs
    it "carries the first failure of a traversable" $ property $ \(xs :: [Either Int Int]) ->
      neg xs === (case [e | Left e <- xs] of { e : _ -> Right e; [] -> Left [a | Right a <- xs] })
    it "of atLeastOne is noneAtAll" $ property $ \(xs :: [Int]) ->
      neg (atLeastOne xs) === noneAtAll xs
    it "turns anySucceeds into all-failed" $ property $ \(xs :: [Either Int Int]) ->
      isRight (neg (anySucceeds xs)) === all (isLeft . bifurcate) xs
