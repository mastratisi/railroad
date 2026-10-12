{-# LANGUAGE GHC2021 #-}
module Core where

import Test.QuickCheck hiding (Success, Failure)

type UserId = Int
type AssetId = Int
type Email = String
type Token = String

data Err = BadRequest [String] | Forbidden | NotFound | Conflict | Internal | Unavailable | TooManyRequests
  deriving (Eq, Show)
data DbError = DbError deriving (Eq, Show)
data TokenError = TokenError deriving (Eq, Show)
data Cap = Read | Write deriving (Eq, Show)
data User = User { uid :: UserId, email :: Email, uname :: String } deriving (Eq, Show)
newtype Prefs = Prefs { theme :: String } deriving (Eq, Show)
data ShareReport = ShareReport { granted :: Int, unresolved :: Int, skipped :: [Email] } deriving (Eq, Show)

defaultPrefs :: Prefs
defaultPrefs = Prefs "plain"

instance Arbitrary DbError where arbitrary = pure DbError
instance Arbitrary TokenError where arbitrary = pure TokenError
instance Arbitrary User where
  arbitrary = User <$> elements [1, 2, 3] <*> email' <*> elements ["ann", "bo"]
instance Arbitrary Prefs where arbitrary = Prefs <$> elements ["dark", "light"]

email' :: Gen Email
email' = elements ["a@x", "b@x", "c@x"]
