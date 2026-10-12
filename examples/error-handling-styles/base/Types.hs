{-# LANGUAGE GHC2021, DataKinds, LambdaCase #-}
module Types (module Core, module Types) where

import Core
import Data.Maybe (fromMaybe)
import Data.Validation
import Effectful
import Effectful.State.Static.Local
import Test.QuickCheck hiding (Success, Failure)

validateName :: String -> Validation [String] String
validateName n = n <$ check (not (null n)) ("empty name: " ++ show n)
                   <* check (length n <= 10) ("too long: " ++ n)
                   <* check (' ' `notElem` n) ("has space: " ++ n)
  where check ok msg = if ok then Success () else Failure [msg]

-- | The queries the handlers use. Each returns the raw result, as a Rel8 runner would.
data Db es = Db
  { usersById    :: UserId -> Eff es (Either DbError [User])
  , usersByEmail :: Email  -> Eff es (Either DbError [User])
  , permitted    :: Cap -> UserId -> AssetId -> Eff es (Either DbError [Bool])
  , insertGrant  :: UserId -> AssetId -> Eff es (Either DbError ())
  , cacheLookup  :: UserId -> Eff es (Maybe User)
  , prefsOf      :: UserId -> Eff es (Maybe Prefs)
  , mintToken    :: Email -> Eff es (Either TokenError Token)
  , quotaLeft    :: UserId -> Eff es (Either DbError Int)
  , normaliseA   :: Email -> Eff es (Either [String] Email)
  , normaliseB   :: Email -> Eff es (Either [String] Email)
  , audit        :: String -> Eff es ()
  }

-- | A random state of the world: what each query returns.
data World = World
  { wById :: Either DbError [User], wByEmailDefault :: Either DbError [User]
  , wByEmail :: [(Email, Either DbError [User])]
  , wWrite :: Either DbError [Bool], wRead :: Either DbError [Bool]
  , wInsert :: Either DbError ()
  , wCache :: Maybe User, wPrefs :: Maybe Prefs
  , wToken :: Either TokenError Token, wQuota :: Either DbError Int
  , wNormA :: Either [String] Email, wNormB :: Either [String] Email
  } deriving Show

mkDb :: State [String] :> es => World -> Db es
mkDb w = Db
  { usersById    = \_ -> pure (wById w)
  , usersByEmail = \e -> pure (fromMaybe (wByEmailDefault w) (lookup e (wByEmail w)))
  , permitted    = \c _ _ -> pure (if c == Write then wWrite w else wRead w)
  , insertGrant  = \_ _ -> pure (wInsert w)
  , cacheLookup  = \_ -> pure (wCache w)
  , prefsOf      = \_ -> pure (wPrefs w)
  , mintToken    = \_ -> pure (wToken w)
  , quotaLeft    = \_ -> pure (wQuota w)
  , normaliseA   = \_ -> pure (wNormA w)
  , normaliseB   = \_ -> pure (wNormB w)
  , audit        = \s -> modify (++ [s])
  }

instance Arbitrary World where
  arbitrary = World
    <$> rows <*> rows <*> listOf ((,) <$> email' <*> rows)
    <*> bools <*> bools <*> arbitrary
    <*> arbitrary <*> arbitrary
    <*> arbitrary <*> oneof [Left <$> arbitrary, Right <$> choose (-1, 2)]
    <*> prob <*> prob
    where
      rows  = frequency [(1, Left <$> arbitrary), (6, Right . pure <$> arbitrary), (2, Right <$> resize 2 (listOf arbitrary))]
      bools = frequency [(1, Left <$> arbitrary), (8, pure (Right [True])), (4, pure (Right [False])), (1, Right <$> resize 2 (listOf arbitrary))]
      prob  = oneof [Left <$> (resize 2 (listOf (elements ["bad scheme", "bad host"]))), Right <$> email']
