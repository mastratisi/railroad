{-# LANGUAGE GHC2021, DataKinds #-}
module H where
import Effectful
import Effectful.Error.Dynamic
import Effectful.State.Static.Local
import Types

type S = '[Error Err, State [String]]

run :: Eff S a -> (Either Err a, [String])
run = runPureEff . runState [] . runErrorNoCallStack

data Handlers = Handlers
  { hGetUser     :: Db S -> UserId -> Eff S User
  , hRegister    :: Db S -> Email -> Eff S Token
  , hCheckWrite  :: Db S -> UserId -> AssetId -> Eff S ()
  , hFindUser    :: Db S -> UserId -> Email -> Eff S User
  , hGrantRead   :: Db S -> UserId -> AssetId -> Email -> Eff S ()
  , hUploadNames :: [String] -> Eff S [String]
  , hPrefsFor    :: Db S -> UserId -> Eff S Prefs
  , hDisplayName :: Db S -> UserId -> Eff S String
  , hUseQuota    :: Db S -> UserId -> Eff S Int
  , hCanonical   :: Db S -> Email -> Eff S Email
  , hShare       :: Db S -> UserId -> AssetId -> [Email] -> Eff S ShareReport
  }

-- | What each handler should return, as shown strings, given a world.
data Oracle = Oracle
  { oGetUser     :: World -> UserId -> String
  , oRegister    :: World -> Email -> String
  , oCheckWrite  :: World -> UserId -> AssetId -> String
  , oFindUser    :: World -> UserId -> Email -> String
  , oGrantRead   :: World -> UserId -> AssetId -> Email -> String
  , oUploadNames :: [String] -> String
  , oPrefsFor    :: World -> UserId -> String
  , oDisplayName :: World -> UserId -> String
  , oUseQuota    :: World -> UserId -> String
  , oCanonical   :: World -> Email -> String
  , oShare       :: World -> UserId -> AssetId -> [Email] -> String
  }
