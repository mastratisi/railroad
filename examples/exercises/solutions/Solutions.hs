{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
-- | The answer key. Do not read this before you have tried the exercise.
module Solutions where

import Control.Monad (void)
import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Validation (Validation (..))
import Domain
import Railroad.MonadError

ex00 :: MonadError Err m => m (Either DbError User) -> m User
ex00 q = q ? Unavailable

ex01 :: MonadError Err m => m (Maybe User) -> m User
ex01 q = q ? NotFound

ex02 :: MonadError Err m => m Bool -> m ()
ex02 q = q ? Forbidden

ex03 :: MonadError Err m => m (Either TokenError Token) -> m Token
ex03 q = q ? Internal

ex04 :: MonadError Err m => m (Either DbError User) -> m Bool -> m User
ex04 q perm = do
  u <- q ? Unavailable
  perm ? Forbidden
  pure u

ex05 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ex05 q = q ? Unavailable ? NotFound

ex06 :: MonadError Err m => m (Maybe (Either DbError User)) -> m User
ex06 q = q ? NotFound ? Unavailable

ex07 :: MonadError Err m => m (Either DbError Bool) -> m ()
ex07 q = q ? Unavailable ? Forbidden

ex08 :: MonadError Err m => m (Either DbError User) -> m User
ex08 q = q ?? fromDb

ex09 :: MonadError Err m => m (Either [String] User) -> m User
ex09 q = q ?? BadRequest

ex10 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ex10 q = q ?? fromDb ? NotFound

ex11 :: MonadError Err m => m (Either DbError [User]) -> m User
ex11 q = q ? Unavailable ?! cardinalityErr NotFound (const Internal)

ex12 :: MonadError Err m => m [User] -> m [User]
ex12 q = q ?+ NotFound

ex13 :: MonadError Err m => m (Either DbError [User]) -> m ()
ex13 q = q ? Unavailable ?∅ const Conflict

ex14 :: MonadError Err m => m (Either DbError [User]) -> m User
ex14 q = q ? Unavailable ?! cardinalityErr NotFound (Ambiguous . map name)

ex15 :: MonadError Err m => m (Either DbError [User]) -> m User
ex15 q = q ?? fromDb ?! cardinalityErr NotFound (const Internal)

ex16 :: MonadError Err m => m (Either DbError [User]) -> m (Either TokenError Token) -> m Token
ex16 usersQ mint = do
  usersQ ?? fromDb ?∅ const Conflict
  mint ? Internal

ex17 :: MonadError Err m => m (Either DbError (Maybe Bool)) -> m ()
ex17 q = q ? Unavailable ? Forbidden ? Forbidden

ex18 :: Monad m => m (Maybe Prefs) -> m Prefs
ex18 q = q ?~ defaultPrefs

ex19 :: Monad m => m (Either DbError Int) -> m Int
ex19 q = q ?~ 0

ex20 :: Monad m => m (Either DbError Int) -> m Int
ex20 q = q ??~ \case { DbDown -> -1; DbTimeout -> -2 }

ex21 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ex21 logIt q = q ?<* (\_ -> logIt "db failed") ? Unavailable

ex22 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ex22 logIt q = q ?<* (logIt . show) ?? fromDb

ex23 :: MonadError Err m => m [Maybe User] -> m [User]
ex23 q = q ? NotFound

ex24 :: MonadError Err m => m [Either DbError User] -> m [User]
ex24 q = q ?? fromDb

ex25 :: MonadError Err m => m (Either DbError [Bool]) -> m ()
ex25 q = void (q ? Unavailable ?+ Forbidden ? Forbidden)

ex26 :: MonadError Err m => m [Validation [String] String] -> m [String]
ex26 q = q ?? BadRequest

ex27 :: MonadError Err m => m [Either [String] String] -> m [String]
ex27 q = q ?? BadRequest

ex28 :: MonadError Err m => m (Maybe User) -> m (Either DbError User) -> m User
ex28 cache db = cache ?| db ? Unavailable

ex29 :: MonadError Err m => m (Maybe User) -> m (Maybe User) -> m (Either DbError User) -> m User
ex29 c1 c2 db = c1 ?| c2 ?| db ? Unavailable

ex30 :: MonadError Err m => m (Either [String] String) -> m (Either [String] String) -> m String
ex30 a b = a ?|<> b ?? BadRequest

ex31 :: MonadError Err m => m (Either DbError Int) -> m Int
ex31 q = q ? Unavailable ?> (> 0) ? TooManyRequests

ex32 :: MonadError Err m => Int -> m (Either DbError Int) -> m Int
ex32 limit q = q ? Unavailable ?> (<= limit) ?? QuotaExceeded

ex33 :: MonadError Err m => (String -> m ()) -> m (Maybe User) -> m (Either DbError User) -> m User
ex33 logIt primary backup = primary ?<* (\_ -> logIt "primary failed") ?| backup ? Unavailable

ex34 :: MonadError Err m => m (Maybe User) -> m ()
ex34 q = q <&> neg ? Conflict

ex35 :: MonadError Err m => m [Either DbError User] -> m [User]
ex35 q = q ?∃ const NotFound

ex36 :: MonadError Err m => m (Either DbError [User]) -> m User
ex36 q = q ? Unavailable <&> exactlyOne ?~ guest

ex37 :: MonadError Err m
     => m (Maybe User) -> m (Either DbError [User]) -> m (Either DbError [User]) -> m User
ex37 cache byId byEmail =
  cache
    ?| one byId
    ?| one byEmail
    ?? cardinalityErr NotFound (const Internal)
  where
    one q = q ?? fromDb <&> exactlyOne

ex38 :: MonadError Err m
     => (String -> m ()) -> m (Either DbError [Bool]) -> m (Either DbError [User])
     -> m (Either DbError [Bool]) -> m (Either DbError ()) -> m ()
ex38 logIt writeQ targetQ readQ insertQ = do
  writeQ ?<* (\_ -> logIt "permission query failed")
    ? Internal ?! cardinalityErr Internal (const Internal) ? Forbidden
  _ <- targetQ ?? fromDb ?! cardinalityErr NotFound (const Internal)
  readQ ? Internal ?! cardinalityErr Internal (const Internal) <&> neg ? Conflict
  insertQ ? Internal
