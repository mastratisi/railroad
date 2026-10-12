{-# LANGUAGE GHC2021, DataKinds, LambdaCase #-}
module Helper where

import Control.Monad (unless, when)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Traversable (for)
import Data.Validation
import Effectful
import Effectful.Error.Dynamic
import Types

orThrow :: Error Err :> es => Err -> Eff es (Either e a) -> Eff es a
orThrow err q = q >>= either (const (throwError err)) pure

expectOne :: Error Err :> es => Err -> Err -> [a] -> Eff es a
expectOne onNone onMany = \case
  []  -> throwError onNone
  [x] -> pure x
  _   -> throwError onMany

single :: [a] -> Maybe a
single [x] = Just x
single _   = Nothing

getUser :: Error Err :> es => Db es -> UserId -> Eff es User
getUser db i = orThrow Unavailable (usersById db i) >>= expectOne NotFound Internal

register :: Error Err :> es => Db es -> Email -> Eff es Token
register db e = do
  rows <- orThrow Unavailable (usersByEmail db e)
  unless (null rows) (throwError Conflict)
  orThrow Internal (mintToken db e)

checkWrite :: Error Err :> es => Db es -> UserId -> AssetId -> Eff es ()
checkWrite db u a = do
  ok <- orThrow Internal (permitted db Write u a) >>= expectOne Internal Internal
  unless ok (throwError Forbidden)

findUser :: Error Err :> es => Db es -> UserId -> Email -> Eff es User
findUser db i e = do
  cached <- cacheLookup db i
  case cached of
    Just u  -> pure u
    Nothing -> do
      byId <- single <$> orThrow Unavailable (usersById db i)
      case byId of
        Just u  -> pure u
        Nothing -> orThrow Unavailable (usersByEmail db e) >>= expectOne NotFound Internal

grantRead :: Error Err :> es => Db es -> UserId -> AssetId -> Email -> Eff es ()
grantRead db author a e = do
  rw <- permitted db Write author a
  when (isLeft rw) (audit db "permission query failed")
  mayWrite <- orThrow Internal (pure rw) >>= expectOne Internal Internal
  unless mayWrite (throwError Forbidden)
  target <- orThrow Unavailable (usersByEmail db e) >>= expectOne NotFound Internal
  already <- orThrow Internal (permitted db Read (uid target) a) >>= expectOne Internal Internal
  when already (throwError Conflict)
  orThrow Internal (insertGrant db (uid target) a)

uploadNames :: Error Err :> es => [String] -> Eff es [String]
uploadNames names = case traverse validateName names of
  Failure ps -> throwError (BadRequest ps)
  Success ns -> pure ns

prefsFor :: Db es -> UserId -> Eff es Prefs
prefsFor db i = fromMaybe defaultPrefs <$> prefsOf db i

displayName :: Error Err :> es => Db es -> UserId -> Eff es String
displayName db i = do
  rows <- orThrow Unavailable (usersById db i)
  pure $ case rows of
    []  -> "(unknown)"
    [u] -> uname u
    _   -> "(ambiguous)"

useQuota :: Error Err :> es => Db es -> UserId -> Eff es Int
useQuota db i = do
  n <- orThrow Unavailable (quotaLeft db i)
  if n > 0 then pure n else throwError TooManyRequests

canonical :: Error Err :> es => Db es -> Email -> Eff es Email
canonical db e = do
  ra <- normaliseA db e
  case ra of
    Right x -> pure x
    Left ea -> do
      rb <- normaliseB db e
      case rb of
        Right x -> pure x
        Left eb -> throwError (BadRequest (ea <> eb))

shareWithMany :: Error Err :> es => Db es -> UserId -> AssetId -> [Email] -> Eff es ShareReport
shareWithMany db author a emails = do
  when (null emails) (throwError (BadRequest ["no emails"]))
  checkWrite db author a
  found <- for emails $ \e -> single <$> orThrow Unavailable (usersByEmail db e)
  let users = catMaybes found
  when (null users) (throwError NotFound)
  for_ users $ \u -> orThrow Internal (insertGrant db (uid u) a)
  pure ShareReport { granted = length users
                   , unresolved = length [() | Nothing <- found]
                   , skipped = [e | (e, Nothing) <- zip emails found] }
