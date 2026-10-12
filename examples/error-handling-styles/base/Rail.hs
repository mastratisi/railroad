{-# LANGUAGE GHC2021, DataKinds #-}
module Rail where

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Validation
import Effectful
import Effectful.Error.Dynamic
import Railroad
import Types

-- DB failure first, then exactly one row.
one :: Error Err :> es => Eff es (Either e [a]) -> Eff es (Either (CardinalityError [a]) a)
one q = q ? Unavailable <&> exactlyOne

getUser :: Error Err :> es => Db es -> UserId -> Eff es User
getUser db i = usersById db i ? Unavailable ?! cardinalityErr NotFound (const Internal)

register :: Error Err :> es => Db es -> Email -> Eff es Token
register db e = do
  usersByEmail db e ? Unavailable ?∅ const Conflict
  mintToken db e ? Internal

checkWrite :: Error Err :> es => Db es -> UserId -> AssetId -> Eff es ()
checkWrite db u a = permitted db Write u a ? Internal ?! cardinalityErr Internal (const Internal) ? Forbidden

findUser :: Error Err :> es => Db es -> UserId -> Email -> Eff es User
findUser db i e =
  cacheLookup db i
    ?| one (usersById db i)
    ?| one (usersByEmail db e)
    ?? cardinalityErr NotFound (const Internal)

grantRead :: Error Err :> es => Db es -> UserId -> AssetId -> Email -> Eff es ()
grantRead db author a e = do
  permitted db Write author a ?<* (\_ -> audit db "permission query failed")
    ? Internal ?! cardinalityErr Internal (const Internal) ? Forbidden
  target <- one (usersByEmail db e) ?? cardinalityErr NotFound (const Internal)
  permitted db Read (uid target) a ? Internal ?! cardinalityErr Internal (const Internal) <&> neg ? Conflict
  insertGrant db (uid target) a ? Internal

uploadNames :: Error Err :> es => [String] -> Eff es [String]
uploadNames names = pure (map validateName names) ?? BadRequest

prefsFor :: Db es -> UserId -> Eff es Prefs
prefsFor db i = prefsOf db i ?~ defaultPrefs

displayName :: Error Err :> es => Db es -> UserId -> Eff es String
displayName db i = (one (usersById db i) <&> fmap uname) ??~ cardinalityErr "(unknown)" (const "(ambiguous)")

useQuota :: Error Err :> es => Db es -> UserId -> Eff es Int
useQuota db i = quotaLeft db i ? Unavailable ?> (> 0) ? TooManyRequests

canonical :: Error Err :> es => Db es -> Email -> Eff es Email
canonical db e = normaliseA db e ?|<> normaliseB db e ?? BadRequest

shareWithMany :: Error Err :> es => Db es -> UserId -> AssetId -> [Email] -> Eff es ShareReport
shareWithMany db author a emails = do
  _ <- pure emails ?+ BadRequest ["no emails"]
  checkWrite db author a
  found <- traverse (one . usersByEmail db) emails
  users <- pure found ?∃ const NotFound
  traverse_ (\u -> insertGrant db (uid u) a ? Internal) users
  pure ShareReport { granted = length users
                   , unresolved = length (failures found)
                   , skipped = [e | (e, Left _) <- zip emails found] }
