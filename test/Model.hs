{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Shared by the specs: generators, the shapes the laws are run over, and helpers.
module Model
  ( Shape, forShapes
  ) where

import           Data.Validation    (Validation (..))
import           Data.Proxy         (Proxy (..))
import           Railroad.Bifurcate
import           Test.Hspec
import           Test.QuickCheck        hiding (Result (..))

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]

-- | Everything a law needs to generate, compare and show a bifurcatable value.
type Shape a =
  ( Bifurcate a, Arbitrary a, Show a
  , Show (CErr a), Eq (CErr a), Function (CErr a), CoArbitrary (CErr a)
  , Show (CRes a), Eq (CRes a), Arbitrary (CRes a) )

-- | Run one property over the base structures, traversables of them, and layered shapes.
forShapes :: String -> (forall a. Shape a => Proxy a -> Property) -> Spec
forShapes name prop = describe name $ do
  it "Bool"                  $ prop (Proxy @Bool)
  it "Maybe"                 $ prop (Proxy @(Maybe Int))
  it "Either"                $ prop (Proxy @(Either Int Int))
  it "Validation"            $ prop (Proxy @(Validation [Int] Int))
  it "[Bool]"                $ prop (Proxy @[Bool])
  it "[Maybe]"               $ prop (Proxy @[Maybe Int])
  it "[Either]"              $ prop (Proxy @[Either Int Int])
  it "[Validation]"          $ prop (Proxy @[Validation [Int] Int])
  it "Either of Maybe"       $ prop (Proxy @(Either Int (Maybe Int)))
  it "Maybe of Maybe"        $ prop (Proxy @(Maybe (Maybe Int)))
  it "Either of Bool"        $ prop (Proxy @(Either Int Bool))
