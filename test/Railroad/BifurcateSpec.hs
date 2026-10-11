{-# LANGUAGE TypeFamilies #-}

module Railroad.BifurcateSpec where

import           Control.Monad.State    (modify, runState)
import           Data.Bifunctor         (first)
import           Data.Either            (isLeft)
import           Data.Functor.Identity
import           Data.Maybe             (listToMaybe)
import           Data.Proxy             (Proxy (..))
import           Data.Validation        (Validation (..))
import           Model
import           Railroad.Bifurcate
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
