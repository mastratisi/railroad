{-# LANGUAGE GHC2021, DataKinds, LambdaCase #-}
module Case where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Traversable (for)
import Data.Validation
import Effectful
import Effectful.Error.Dynamic
import Types

getUser :: Error Err :> es => Db es -> UserId -> Eff es User
getUser db i = do
  r <- usersById db i
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    []  -> throwError NotFound
    [u] -> pure u
    _   -> throwError Internal

register :: Error Err :> es => Db es -> Email -> Eff es Token
register db e = do
  r <- usersByEmail db e
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    [] -> pure ()
    _  -> throwError Conflict
  t <- mintToken db e
  case t of
    Left _  -> throwError Internal
    Right x -> pure x

checkWrite :: Error Err :> es => Db es -> UserId -> AssetId -> Eff es ()
checkWrite db u a = do
  r <- permitted db Write u a
  rows <- case r of
    Left _  -> throwError Internal
    Right x -> pure x
  ok <- case rows of
    [b] -> pure b
    _   -> throwError Internal
  case ok of
    True  -> pure ()
    False -> throwError Forbidden

findUser :: Error Err :> es => Db es -> UserId -> Email -> Eff es User
findUser db i e = do
  cached <- cacheLookup db i
  case cached of
    Just u  -> pure u
    Nothing -> do
      r1 <- usersById db i
      byId <- case r1 of
        Left _  -> throwError Unavailable
        Right x -> pure x
      case byId of
        [u] -> pure u
        _   -> do
          r2 <- usersByEmail db e
          byEmail <- case r2 of
            Left _  -> throwError Unavailable
            Right x -> pure x
          case byEmail of
            []  -> throwError NotFound
            [u] -> pure u
            _   -> throwError Internal

grantRead :: Error Err :> es => Db es -> UserId -> AssetId -> Email -> Eff es ()
grantRead db author a e = do
  rw <- permitted db Write author a
  wrows <- case rw of
    Left _  -> audit db "permission query failed" >> throwError Internal
    Right x -> pure x
  mayWrite <- case wrows of
    [b] -> pure b
    _   -> throwError Internal
  case mayWrite of
    True  -> pure ()
    False -> throwError Forbidden
  r <- usersByEmail db e
  urows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  target <- case urows of
    []  -> throwError NotFound
    [u] -> pure u
    _   -> throwError Internal
  rr <- permitted db Read (uid target) a
  rrows <- case rr of
    Left _  -> throwError Internal
    Right x -> pure x
  already <- case rrows of
    [b] -> pure b
    _   -> throwError Internal
  case already of
    True  -> throwError Conflict
    False -> pure ()
  ri <- insertGrant db (uid target) a
  case ri of
    Left _  -> throwError Internal
    Right x -> pure x

uploadNames :: Error Err :> es => [String] -> Eff es [String]
uploadNames names = case traverse validateName names of
  Failure ps -> throwError (BadRequest ps)
  Success ns -> pure ns

prefsFor :: Db es -> UserId -> Eff es Prefs
prefsFor db i = do
  m <- prefsOf db i
  pure $ case m of
    Just p  -> p
    Nothing -> defaultPrefs

displayName :: Error Err :> es => Db es -> UserId -> Eff es String
displayName db i = do
  r <- usersById db i
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  pure $ case rows of
    []  -> "(unknown)"
    [u] -> uname u
    _   -> "(ambiguous)"

useQuota :: Error Err :> es => Db es -> UserId -> Eff es Int
useQuota db i = do
  r <- quotaLeft db i
  n <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case n > 0 of
    True  -> pure n
    False -> throwError TooManyRequests

canonical :: Error Err :> es => Db es -> Email -> Eff es Email
canonical db e = do
  ra <- normaliseA db e
  case ra of
    Right x  -> pure x
    Left ea -> do
      rb <- normaliseB db e
      case rb of
        Right x  -> pure x
        Left eb -> throwError (BadRequest (ea <> eb))

shareWithMany :: Error Err :> es => Db es -> UserId -> AssetId -> [Email] -> Eff es ShareReport
shareWithMany db author a emails = do
  case emails of
    [] -> throwError (BadRequest ["no emails"])
    _  -> pure ()
  checkWrite db author a
  found <- for emails $ \e -> do
    r <- usersByEmail db e
    rows <- case r of
      Left _  -> throwError Unavailable
      Right x -> pure x
    pure $ case rows of
      [u] -> Right u
      _   -> Left ()
  let users = [u | Right u <- found]
  case users of
    [] -> throwError NotFound
    _  -> pure ()
  for_ users $ \u -> do
    ri <- insertGrant db (uid u) a
    case ri of
      Left _  -> throwError Internal
      Right x -> pure x
  pure ShareReport { granted = length users
                   , unresolved = length [() | Left _ <- found]
                   , skipped = [e | (e, Left _) <- zip emails found] }
