{-# LANGUAGE GHC2021 #-}
-- | The vocabulary every exercise shares. Nothing to do here.
module Domain where

import Data.Validation (Validation (..))
import Test.QuickCheck hiding (Failure, Success)

-- | What a handler can answer with when it gives up.
data Err
  = NotFound
  | Conflict
  | Forbidden
  | Unavailable
  | GatewayTimeout
  | Internal
  | TooManyRequests
  | BadRequest [String]
  | Ambiguous [String]
  | QuotaExceeded Int
  deriving (Eq, Show)

-- | Why a database call failed.
data DbError = DbDown | DbTimeout
  deriving (Eq, Show)

-- | Why minting a token failed.
data TokenError = TokenError
  deriving (Eq, Show)

data User = User {uid :: Int, name :: String}
  deriving (Eq, Show)

newtype Prefs = Prefs String
  deriving (Eq, Show)

type Token = String

defaultPrefs :: Prefs
defaultPrefs = Prefs "plain"

guest :: User
guest = User 0 "guest"

-- | Database failure to response: down is 503, timeout is 504.
fromDb :: DbError -> Err
fromDb DbDown    = Unavailable
fromDb DbTimeout = GatewayTimeout

instance Arbitrary DbError where arbitrary = elements [DbDown, DbTimeout]
instance Arbitrary TokenError where arbitrary = pure TokenError
instance Arbitrary User where
  arbitrary = User <$> elements [1, 2, 3] <*> elements ["ann", "bo", "cy"]
instance Arbitrary Prefs where arbitrary = Prefs <$> elements ["dark", "light"]
instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Failure <$> arbitrary, Success <$> arbitrary]
