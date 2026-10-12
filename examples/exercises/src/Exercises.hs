{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fdefer-typed-holes -Wno-unused-imports -Wno-unused-matches -Wno-typed-holes #-}

-- | Exercises for the railroad combinators.
--
-- Every exercise has two functions:
--
--   * @refNN@, written with plain @case@. It is correct. Read it.
--   * @exNN@, a stub. Write it with the railroad operators so that it behaves exactly like @refNN@.
--
-- Edit only the @exNN@ definitions. Run @cabal test exercises@ from this directory; each exercise
-- is one test, and the test feeds both functions the same random inputs.
--
-- Rules of the game:
--
--   * Try before you look things up. @HINTS.md@ has three hints per exercise, vaguest first.
--   * Do not take the query result apart with @case@, @either@, @maybe@ or @if@. The point is to let
--     the operators do it. (A function that maps a failure, such as @\\case@ for exercise 20, is fine.)
--   * Do the lessons in order. Lessons 5 and 11 are reviews: they mix what came before and do not
--     say which operator to use. Lesson 13 asks for whole handlers.
--
-- A stub that says @todo@ fails with "TODO". A stub with a @_@ is a hole for you to fill; it also
-- fails until you do.
module Exercises where

import Control.Monad.Except
import Data.Functor ((<&>))
import Data.Validation (Validation (..))
import Domain
import Railroad.MonadError

todo :: a
todo = error "TODO: write this with the railroad operators"

-- ============================================================================================
-- Lesson 1: unwrap, or throw a constant                                                  (?)
-- ============================================================================================
--
-- `m ? e` runs `m`, and if the result is a failure, throws `e`. Otherwise it continues with the
-- unwrapped success. A failure is `Left _`, `Nothing` or `False`.

-- ---- 00. Worked example (already solved; read it, then the test for it is green) -----------
--
-- A query returns Either DbError User. Failure is 503 (Unavailable).

ref00 :: MonadError Err m => m (Either DbError User) -> m User
ref00 q = do
  r <- q
  case r of
    Left _  -> throwError Unavailable
    Right u -> pure u

ex00 :: MonadError Err m => m (Either DbError User) -> m User
ex00 q = q ? Unavailable

-- ---- 01. Maybe (faded: the operator is given, fill the error) ------------------------------
--
-- No user found is 404 (NotFound).

ref01 :: MonadError Err m => m (Maybe User) -> m User
ref01 q = do
  r <- q
  case r of
    Nothing -> throwError NotFound
    Just u  -> pure u

ex01 :: MonadError Err m => m (Maybe User) -> m User
ex01 q = q ? _

-- ---- 02. Bool ---------------------------------------------------------------------------------
--
-- A permission check answers True or False. False is 403 (Forbidden). Return nothing.

ref02 :: MonadError Err m => m Bool -> m ()
ref02 q = do
  ok <- q
  if ok then pure () else throwError Forbidden

ex02 :: MonadError Err m => m Bool -> m ()
ex02 = todo

-- ---- 03. Either, another one -----------------------------------------------------------------
--
-- Minting a token can fail. That is 500 (Internal).

ref03 :: MonadError Err m => m (Either TokenError Token) -> m Token
ref03 q = do
  r <- q
  case r of
    Left _  -> throwError Internal
    Right t -> pure t

ex03 :: MonadError Err m => m (Either TokenError Token) -> m Token
ex03 = todo

-- ---- 04. Two steps ---------------------------------------------------------------------------
--
-- Fetch the user (503 if the query fails), then check the permission (403 if False). Return the
-- user. If the first step fails, the second must not run.

ref04 :: MonadError Err m => m (Either DbError User) -> m Bool -> m User
ref04 q perm = do
  r <- q
  u <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  ok <- perm
  case ok of
    True  -> pure u
    False -> throwError Forbidden

ex04 :: MonadError Err m => m (Either DbError User) -> m Bool -> m User
ex04 = todo

-- ============================================================================================
-- Lesson 2: layers, unwrapped left to right
-- ============================================================================================
--
-- When a result has a layer inside a layer, chain the operator: one `?` per layer, outermost
-- first.

-- ---- 05. Either, then Maybe ------------------------------------------------------------------
--
-- The query can fail (503). If it works it may find nobody (404).

ref05 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ref05 q = do
  r <- q
  case r of
    Left _          -> throwError Unavailable
    Right Nothing   -> throwError NotFound
    Right (Just u)  -> pure u

ex05 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ex05 = todo

-- ---- 06. Maybe, then Either (look at which layer is outside) --------------------------------
--
-- A cache may have no entry (404). If it has one, the entry itself may be a failure (503).

ref06 :: MonadError Err m => m (Maybe (Either DbError User)) -> m User
ref06 q = do
  r <- q
  case r of
    Nothing          -> throwError NotFound
    Just (Left _)    -> throwError Unavailable
    Just (Right u)   -> pure u

ex06 :: MonadError Err m => m (Maybe (Either DbError User)) -> m User
ex06 = todo

-- ---- 07. Either, then Bool -------------------------------------------------------------------
--
-- The permission query can fail (503). If it works, False means 403.

ref07 :: MonadError Err m => m (Either DbError Bool) -> m ()
ref07 q = do
  r <- q
  case r of
    Left _      -> throwError Unavailable
    Right True  -> pure ()
    Right False -> throwError Forbidden

ex07 :: MonadError Err m => m (Either DbError Bool) -> m ()
ex07 = todo

-- ============================================================================================
-- Lesson 3: the error depends on what failed                                              (??)
-- ============================================================================================
--
-- `m ?? f` is like `?`, but the error is `f` applied to the failure. `f` can be any function,
-- including a constructor. (Domain has `fromDb :: DbError -> Err`.)

-- ---- 08. Database failures map to different codes --------------------------------------------

ref08 :: MonadError Err m => m (Either DbError User) -> m User
ref08 q = do
  r <- q
  case r of
    Left DbDown    -> throwError Unavailable
    Left DbTimeout -> throwError GatewayTimeout
    Right u        -> pure u

ex08 :: MonadError Err m => m (Either DbError User) -> m User
ex08 = todo

-- ---- 09. The failure carries the problems ----------------------------------------------------
--
-- A parse can fail with a list of messages. Those become the body of a 400 (BadRequest).

ref09 :: MonadError Err m => m (Either [String] User) -> m User
ref09 q = do
  r <- q
  case r of
    Left problems -> throwError (BadRequest problems)
    Right u       -> pure u

ex09 :: MonadError Err m => m (Either [String] User) -> m User
ex09 = todo

-- ---- 10. A mapped error, then a constant -----------------------------------------------------
--
-- The database failure maps with `fromDb`; a missing user is 404.

ref10 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ref10 q = do
  r <- q
  case r of
    Left e         -> throwError (fromDb e)
    Right Nothing  -> throwError NotFound
    Right (Just u) -> pure u

ex10 :: MonadError Err m => m (Either DbError (Maybe User)) -> m User
ex10 = todo

-- ============================================================================================
-- Lesson 4: how many rows?                                          (?!)  (?+)  (?∅)
-- ============================================================================================
--
-- A list of rows is not a failure by itself, but the number of rows can be:
--   `?+  e`   at least one row; keeps the rows.
--   `?! f`    exactly one row; gives the row. `f` gets `cardinalityErr onNone onMany`.
--   `?∅  f`   no rows (the ASCII spelling is `?@`); `f` gets the rows.
-- `cardinalityErr :: e -> (rows -> e) -> CardinalityError rows -> e` builds the error.

-- ---- 11. Exactly one -------------------------------------------------------------------------
--
-- Query fails: 503. No row: 404. More than one: 500.

ref11 :: MonadError Err m => m (Either DbError [User]) -> m User
ref11 q = do
  r <- q
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    []  -> throwError NotFound
    [u] -> pure u
    _   -> throwError Internal

ex11 :: MonadError Err m => m (Either DbError [User]) -> m User
ex11 = todo

-- ---- 12. At least one ------------------------------------------------------------------------
--
-- A search with no hits is 404. Otherwise return the hits.

ref12 :: MonadError Err m => m [User] -> m [User]
ref12 q = do
  hits <- q
  case hits of
    [] -> throwError NotFound
    _  -> pure hits

ex12 :: MonadError Err m => m [User] -> m [User]
ex12 = todo

-- ---- 13. None --------------------------------------------------------------------------------
--
-- Registering: an account with this email must not exist. Query fails: 503. Any row: 409.

ref13 :: MonadError Err m => m (Either DbError [User]) -> m ()
ref13 q = do
  r <- q
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    [] -> pure ()
    _  -> throwError Conflict

ex13 :: MonadError Err m => m (Either DbError [User]) -> m ()
ex13 = todo

-- ---- 14. The error carries the rows ----------------------------------------------------------
--
-- Like 11, but when several users match, the error lists their names (Ambiguous).

ref14 :: MonadError Err m => m (Either DbError [User]) -> m User
ref14 q = do
  r <- q
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    []  -> throwError NotFound
    [u] -> pure u
    us  -> throwError (Ambiguous (map name us))

ex14 :: MonadError Err m => m (Either DbError [User]) -> m User
ex14 = todo

-- ============================================================================================
-- Lesson 5: review A. Nothing here says which operator to use.
-- ============================================================================================

-- ---- 15. Look a user up by email -------------------------------------------------------------
--
-- A failed query maps with `fromDb`. No match is 404; several matches are 500.

ref15 :: MonadError Err m => m (Either DbError [User]) -> m User
ref15 q = do
  r <- q
  rows <- case r of
    Left e  -> throwError (fromDb e)
    Right x -> pure x
  case rows of
    []  -> throwError NotFound
    [u] -> pure u
    _   -> throwError Internal

ex15 :: MonadError Err m => m (Either DbError [User]) -> m User
ex15 = todo

-- ---- 16. Register ----------------------------------------------------------------------------
--
-- The email must not belong to anyone (the query's failure maps with `fromDb`; any row is 409).
-- Then mint the token; if that fails, 500. The token step must not run after an earlier failure.

ref16 :: MonadError Err m => m (Either DbError [User]) -> m (Either TokenError Token) -> m Token
ref16 usersQ mint = do
  r <- usersQ
  rows <- case r of
    Left e  -> throwError (fromDb e)
    Right x -> pure x
  case rows of
    [] -> pure ()
    _  -> throwError Conflict
  t <- mint
  case t of
    Left _  -> throwError Internal
    Right x -> pure x

ex16 :: MonadError Err m => m (Either DbError [User]) -> m (Either TokenError Token) -> m Token
ex16 = todo

-- ---- 17. A grant that may not exist ----------------------------------------------------------
--
-- Query fails: 503. No grant row: 403. A grant that says False: 403.

ref17 :: MonadError Err m => m (Either DbError (Maybe Bool)) -> m ()
ref17 q = do
  r <- q
  case r of
    Left _            -> throwError Unavailable
    Right Nothing     -> throwError Forbidden
    Right (Just True) -> pure ()
    Right (Just False)-> throwError Forbidden

ex17 :: MonadError Err m => m (Either DbError (Maybe Bool)) -> m ()
ex17 = todo

-- ============================================================================================
-- Lesson 6: recover instead of throwing                                       (?~)  (??~)
-- ============================================================================================
--
-- `m ?~ d` replaces a failure with the default `d`. `m ??~ f` computes the replacement from the
-- failure. Neither throws.

-- ---- 18. A default for a missing value --------------------------------------------------------
--
-- Preferences may be missing. Use `defaultPrefs`.

ref18 :: Monad m => m (Maybe Prefs) -> m Prefs
ref18 q = do
  r <- q
  case r of
    Nothing -> pure defaultPrefs
    Just p  -> pure p

ex18 :: Monad m => m (Maybe Prefs) -> m Prefs
ex18 = todo

-- ---- 19. A default after a failure -----------------------------------------------------------
--
-- A counter lookup that fails counts as 0.

ref19 :: Monad m => m (Either DbError Int) -> m Int
ref19 q = do
  r <- q
  case r of
    Left _  -> pure 0
    Right n -> pure n

ex19 :: Monad m => m (Either DbError Int) -> m Int
ex19 = todo

-- ---- 20. A replacement that depends on the failure --------------------------------------------
--
-- A failed lookup is -1 if the database is down, and -2 if it timed out.

ref20 :: Monad m => m (Either DbError Int) -> m Int
ref20 q = do
  r <- q
  case r of
    Left DbDown    -> pure (-1)
    Left DbTimeout -> pure (-2)
    Right n        -> pure n

ex20 :: Monad m => m (Either DbError Int) -> m Int
ex20 = todo

-- ============================================================================================
-- Lesson 7: look at the failure before it is thrown away                               (?<*)
-- ============================================================================================
--
-- `m ?<* h` runs the handler `h` on a failure and passes the value through unchanged, so a later
-- operator still sees it. A constant error such as `? Unavailable` throws the failure away, so
-- anything that wants to look at it must come first.

-- ---- 21. Log, then throw ---------------------------------------------------------------------
--
-- On a failed query, write "db failed" with the logger, then throw 503. Log nothing on success.

ref21 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ref21 logIt q = do
  r <- q
  case r of
    Left _  -> logIt "db failed" >> throwError Unavailable
    Right u -> pure u

ex21 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ex21 = todo

-- ---- 22. Log what failed ---------------------------------------------------------------------
--
-- On a failed query, log `show` of the failure, then throw `fromDb` of it.

ref22 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ref22 logIt q = do
  r <- q
  case r of
    Left e  -> logIt (show e) >> throwError (fromDb e)
    Right u -> pure u

ex22 :: MonadError Err m => (String -> m ()) -> m (Either DbError User) -> m User
ex22 = todo

-- ============================================================================================
-- Lesson 8: a whole list of results
-- ============================================================================================
--
-- On a list of Maybe or Either, `?` and `??` check every element: ALL must succeed. The first
-- failure is the one you get.

-- ---- 23. Every lookup must have found someone ------------------------------------------------

ref23 :: MonadError Err m => m [Maybe User] -> m [User]
ref23 q = do
  xs <- q
  case sequence xs of
    Nothing -> throwError NotFound
    Just us -> pure us

ex23 :: MonadError Err m => m [Maybe User] -> m [User]
ex23 = todo

-- ---- 24. Every lookup must have worked; report the first failure -----------------------------

ref24 :: MonadError Err m => m [Either DbError User] -> m [User]
ref24 q = do
  xs <- q
  case sequence xs of
    Left e   -> throwError (fromDb e)
    Right us -> pure us

ex24 :: MonadError Err m => m [Either DbError User] -> m [User]
ex24 = todo

-- ---- 25. The trap -----------------------------------------------------------------------------
--
-- Permissions come back as one Bool per grant. Access needs at least one grant, and every grant
-- must be True. Query fails: 503. No grants: 403. Any False: 403.
--
-- Write the obvious thing first and run the test. Then read what it tells you.

ref25 :: MonadError Err m => m (Either DbError [Bool]) -> m ()
ref25 q = do
  r <- q
  grants <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case grants of
    [] -> throwError Forbidden
    _  -> if and grants then pure () else throwError Forbidden

ex25 :: MonadError Err m => m (Either DbError [Bool]) -> m ()
ex25 = todo

-- ============================================================================================
-- Lesson 9: Validation collects every problem
-- ============================================================================================
--
-- A `Validation` is an Either that keeps going: over a list, `?` and `??` hand you ALL the
-- failures, combined, not just the first.

-- ---- 26. Report every problem -----------------------------------------------------------------

ref26 :: MonadError Err m => m [Validation [String] String] -> m [String]
ref26 q = do
  xs <- q
  case traverse id xs of
    Failure ps -> throwError (BadRequest ps)
    Success ns -> pure ns

ex26 :: MonadError Err m => m [Validation [String] String] -> m [String]
ex26 = todo

-- ---- 27. The same, with Either ----------------------------------------------------------------
--
-- Before you run the test, predict: does your solution from 26 work here? Does it do the same
-- thing? Compare `ref26` and `ref27`.

ref27 :: MonadError Err m => m [Either [String] String] -> m [String]
ref27 q = do
  xs <- q
  case sequence xs of
    Left ps -> throwError (BadRequest ps)
    Right ns -> pure ns

ex27 :: MonadError Err m => m [Either [String] String] -> m [String]
ex27 = todo

-- ============================================================================================
-- Lesson 10: try one source, then the next                                     (?|)  (?|<>)
-- ============================================================================================
--
-- `a ?| b` runs `b` only if `a` failed, and keeps the last failure. `a ?|<> b` combines the two
-- failures with `<>`. The result is still a value you finish with `?` or `??`. The sources can
-- have different shapes; they only need the same success type.

-- ---- 28. Cache, then database ----------------------------------------------------------------
--
-- Use the cache's answer if it has one. Otherwise ask the database; if that fails, 503. The
-- database must not be asked when the cache hits.

ref28 :: MonadError Err m => m (Maybe User) -> m (Either DbError User) -> m User
ref28 cache db = do
  c <- cache
  case c of
    Just u  -> pure u
    Nothing -> do
      r <- db
      case r of
        Left _  -> throwError Unavailable
        Right u -> pure u

ex28 :: MonadError Err m => m (Maybe User) -> m (Either DbError User) -> m User
ex28 = todo

-- ---- 29. Three sources -----------------------------------------------------------------------
--
-- Two caches, then the database. Each later source runs only if all earlier ones failed.

ref29 :: MonadError Err m => m (Maybe User) -> m (Maybe User) -> m (Either DbError User) -> m User
ref29 c1 c2 db = do
  a <- c1
  case a of
    Just u  -> pure u
    Nothing -> do
      b <- c2
      case b of
        Just u  -> pure u
        Nothing -> do
          r <- db
          case r of
            Left _  -> throwError Unavailable
            Right u -> pure u

ex29 :: MonadError Err m => m (Maybe User) -> m (Maybe User) -> m (Either DbError User) -> m User
ex29 = todo

-- ---- 30. Combine the problems -----------------------------------------------------------------
--
-- Two normalisers for an email. Use the first that works. If both fail, the 400 lists both
-- sets of problems, the first normaliser's first.

ref30 :: MonadError Err m => m (Either [String] String) -> m (Either [String] String) -> m String
ref30 a b = do
  ra <- a
  case ra of
    Right x -> pure x
    Left ea -> do
      rb <- b
      case rb of
        Right x -> pure x
        Left eb -> throwError (BadRequest (ea <> eb))

ex30 :: MonadError Err m => m (Either [String] String) -> m (Either [String] String) -> m String
ex30 = todo

-- ============================================================================================
-- Lesson 11: review B. Nothing here says which operator to use.
-- ============================================================================================

-- ---- 31. A positive quota ---------------------------------------------------------------------
--
-- The query's failure is 503. A quota of zero or less is 429.

ref31 :: MonadError Err m => m (Either DbError Int) -> m Int
ref31 q = do
  r <- q
  n <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  if n > 0 then pure n else throwError TooManyRequests

ex31 :: MonadError Err m => m (Either DbError Int) -> m Int
ex31 = todo

-- ---- 32. A size limit that names the offender -------------------------------------------------
--
-- Query failure: 503. A size over the limit throws `QuotaExceeded size`.

ref32 :: MonadError Err m => Int -> m (Either DbError Int) -> m Int
ref32 limit q = do
  r <- q
  n <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  if n <= limit then pure n else throwError (QuotaExceeded n)

ex32 :: MonadError Err m => Int -> m (Either DbError Int) -> m Int
ex32 = todo

-- ---- 33. Fetch, then log and fall back --------------------------------------------------------
--
-- Ask the primary source. If it fails, log "primary failed" and ask the backup. If the backup
-- fails too, 503. The backup must not run when the primary works.

ref33 :: MonadError Err m => (String -> m ()) -> m (Maybe User) -> m (Either DbError User) -> m User
ref33 logIt primary backup = do
  p <- primary
  case p of
    Just u  -> pure u
    Nothing -> do
      logIt "primary failed"
      r <- backup
      case r of
        Left _  -> throwError Unavailable
        Right u -> pure u

ex33 :: MonadError Err m => (String -> m ()) -> m (Maybe User) -> m (Either DbError User) -> m User
ex33 = todo

-- ============================================================================================
-- Lesson 12: classify first, then act                     (neg) (?∃) (exactlyOne) (<&>)
-- ============================================================================================
--
-- Some questions are not "did it fail". `xs <&> classifier` turns a value into an Either, and then
-- any operator you know applies to it.
--   neg          swaps failure and success: Nothing passes, Just is a failure.
--   ?∃ f         at least one element succeeds; gives the successes; `f` gets all the failures.
--   exactlyOne   Right the element if there is exactly one, else Left (a CardinalityError).

-- ---- 34. Must not exist ----------------------------------------------------------------------
--
-- Registering: an existing user is 409. No user is fine.

ref34 :: MonadError Err m => m (Maybe User) -> m ()
ref34 q = do
  r <- q
  case r of
    Nothing -> pure ()
    Just _  -> throwError Conflict

ex34 :: MonadError Err m => m (Maybe User) -> m ()
ex34 = todo

-- ---- 35. At least one source answered ----------------------------------------------------------
--
-- Several lookups ran. Keep the users that were found. If none worked, 404.

ref35 :: MonadError Err m => m [Either DbError User] -> m [User]
ref35 q = do
  xs <- q
  case [u | Right u <- xs] of
    [] -> throwError NotFound
    us -> pure us

ex35 :: MonadError Err m => m [Either DbError User] -> m [User]
ex35 = todo

-- ---- 36. Exactly one, or a stand-in ------------------------------------------------------------
--
-- Query failure: 503. Exactly one match: that user. No match or several: `guest`.

ref36 :: MonadError Err m => m (Either DbError [User]) -> m User
ref36 q = do
  r <- q
  rows <- case r of
    Left _  -> throwError Unavailable
    Right x -> pure x
  case rows of
    [u] -> pure u
    _   -> pure guest

ex36 :: MonadError Err m => m (Either DbError [User]) -> m User
ex36 = todo

-- ============================================================================================
-- Lesson 13: capstones. Whole handlers.
-- ============================================================================================

-- ---- 37. Find a user -------------------------------------------------------------------------
--
-- Ask the cache. If it has nobody, ask the database by id; if that does not give exactly one
-- user, ask it by email. A failed query in either database call maps with `fromDb` before any
-- other source is tried. If the email lookup does not find exactly one user: none is 404,
-- several is 500. Each source runs only if the ones before it did not produce a user.
--
-- (Hint, if you want it: write a small local helper for "database call, then exactly one row".)

ref37 :: MonadError Err m
      => m (Maybe User) -> m (Either DbError [User]) -> m (Either DbError [User]) -> m User
ref37 cache byId byEmail = do
  c <- cache
  case c of
    Just u  -> pure u
    Nothing -> do
      r1 <- byId
      rows1 <- case r1 of
        Left e  -> throwError (fromDb e)
        Right x -> pure x
      case rows1 of
        [u] -> pure u
        _   -> do
          r2 <- byEmail
          rows2 <- case r2 of
            Left e  -> throwError (fromDb e)
            Right x -> pure x
          case rows2 of
            []  -> throwError NotFound
            [u] -> pure u
            _   -> throwError Internal

ex37 :: MonadError Err m
     => m (Maybe User) -> m (Either DbError [User]) -> m (Either DbError [User]) -> m User
ex37 = todo

-- ---- 38. Grant read access ---------------------------------------------------------------------
--
-- In order, each step running only if the earlier ones passed:
--
--   1. The caller must be allowed to write. The permission query returns one Bool per grant:
--      if the query fails, call the logger with "permission query failed" and then answer 500;
--      anything but exactly one row is 500; a False is 403.
--   2. The target user must exist: a failed query maps with `fromDb`; no row is 404; several
--      rows are 500.
--   3. The target must not already be allowed to read: a failed query is 500, anything but exactly
--      one row is 500, and a True is 409.
--   4. Insert the grant: a failure is 500.

ref38 :: MonadError Err m
      => (String -> m ()) -> m (Either DbError [Bool]) -> m (Either DbError [User])
      -> m (Either DbError [Bool]) -> m (Either DbError ()) -> m ()
ref38 logIt writeQ targetQ readQ insertQ = do
  rw <- writeQ
  wrows <- case rw of
    Left _  -> logIt "permission query failed" >> throwError Internal
    Right x -> pure x
  mayWrite <- case wrows of
    [b] -> pure b
    _   -> throwError Internal
  case mayWrite of
    True  -> pure ()
    False -> throwError Forbidden
  rt <- targetQ
  urows <- case rt of
    Left e  -> throwError (fromDb e)
    Right x -> pure x
  _target <- case urows of
    []  -> throwError NotFound
    [u] -> pure u
    _   -> throwError Internal
  rr <- readQ
  rrows <- case rr of
    Left _  -> throwError Internal
    Right x -> pure x
  already <- case rrows of
    [b] -> pure b
    _   -> throwError Internal
  case already of
    True  -> throwError Conflict
    False -> pure ()
  ri <- insertQ
  case ri of
    Left _  -> throwError Internal
    Right x -> pure x

ex38 :: MonadError Err m
     => (String -> m ()) -> m (Either DbError [Bool]) -> m (Either DbError [User])
     -> m (Either DbError [Bool]) -> m (Either DbError ()) -> m ()
ex38 = todo
