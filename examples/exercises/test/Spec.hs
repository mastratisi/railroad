{-# LANGUAGE CPP #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
-- | One test per exercise: your @exNN@ must behave exactly like the reference @refNN@ on random
-- inputs. "Behave" includes the log, so a step that runs when it should not is caught.
--
-- You do not need to read or edit this file.
module Main (main) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Validation (Validation)
import Domain
import qualified Exercises as X
#ifdef SOLUTIONS
import qualified Solutions as A
#else
import qualified Exercises as A
#endif
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck hiding (Failure, Success)

-- | The monad every exercise is run in: it can throw an 'Err' and it keeps a log.
type Run = ExceptT Err (Writer [String])

runIt :: Run a -> (Either Err a, [String])
runIt = runWriter . runExceptT

say :: String -> Run ()
say s = lift (tell [s])

-- | An action that records that it ran, then returns a fixed value.
act :: String -> a -> Run a
act s v = say s >> pure v

same :: (Eq a, Show a) => Run a -> Run a -> Property
same x y = runIt x === runIt y

-- ---- generators that make the interesting cases common -------------------------------------

users :: Gen [User]
users = frequency [(2, pure []), (5, (: []) <$> arbitrary), (3, (\a b -> [a, b]) <$> arbitrary <*> arbitrary)]

dbUsers :: Gen (Either DbError [User])
dbUsers = frequency [(1, Left <$> arbitrary), (9, Right <$> users)]

-- | Grants that are mostly one row, mostly the given value.
grantsMostly :: Bool -> Gen (Either DbError [Bool])
grantsMostly b = frequency
  [ (1, Left <$> arbitrary), (1, pure (Right [])), (8, pure (Right [b])), (2, pure (Right [not b]))
  , (1, Right <$> resize 3 (listOf arbitrary)) ]

anyGrants :: Gen (Either DbError [Bool])
anyGrants = frequency
  [ (1, Left <$> arbitrary), (3, pure (Right [])), (5, pure (Right [True])), (2, pure (Right [False]))
  , (4, Right <$> resize 3 (listOf arbitrary)) ]

smallList :: Arbitrary a => Gen [a]
smallList = resize 4 arbitrary

main :: IO ()
main = hspec $ modifyMaxSuccess (const 300) $ do
  describe "Lesson 1: unwrap, or throw a constant" $ do
    it "00 worked example" $ property $ \r -> same (X.ref00 (pure r)) (A.ex00 (pure r))
    it "01 Maybe" $ property $ \r -> same (X.ref01 (pure r)) (A.ex01 (pure r))
    it "02 Bool" $ property $ \r -> same (X.ref02 (pure r)) (A.ex02 (pure r))
    it "03 Either, another one" $ property $ \r -> same (X.ref03 (pure r)) (A.ex03 (pure r))
    it "04 two steps" $ property $ \r b ->
      same (X.ref04 (act "q" r) (act "perm" b)) (A.ex04 (act "q" r) (act "perm" b))

  describe "Lesson 2: layers, unwrapped left to right" $ do
    it "05 Either, then Maybe" $ property $ \r -> same (X.ref05 (pure r)) (A.ex05 (pure r))
    it "06 Maybe, then Either" $ property $ \r -> same (X.ref06 (pure r)) (A.ex06 (pure r))
    it "07 Either, then Bool" $ property $ \r -> same (X.ref07 (pure r)) (A.ex07 (pure r))

  describe "Lesson 3: the error depends on what failed" $ do
    it "08 database failures map to codes" $ property $ \r -> same (X.ref08 (pure r)) (A.ex08 (pure r))
    it "09 the failure carries the problems" $ property $ \r -> same (X.ref09 (pure r)) (A.ex09 (pure r))
    it "10 a mapped error, then a constant" $ property $ \r -> same (X.ref10 (pure r)) (A.ex10 (pure r))

  describe "Lesson 4: how many rows?" $ do
    it "11 exactly one" $ forAll dbUsers $ \r -> same (X.ref11 (pure r)) (A.ex11 (pure r))
    it "12 at least one" $ forAll users $ \r -> same (X.ref12 (pure r)) (A.ex12 (pure r))
    it "13 none" $ forAll dbUsers $ \r -> same (X.ref13 (pure r)) (A.ex13 (pure r))
    it "14 the error carries the rows" $ forAll dbUsers $ \r -> same (X.ref14 (pure r)) (A.ex14 (pure r))

  describe "Lesson 5: review A" $ do
    it "15 look a user up by email" $ forAll dbUsers $ \r -> same (X.ref15 (pure r)) (A.ex15 (pure r))
    it "16 register" $ forAll ((,) <$> dbUsers <*> arbitrary) $ \(r, t) ->
      same (X.ref16 (act "users" r) (act "mint" t)) (A.ex16 (act "users" r) (act "mint" t))
    it "17 a grant that may not exist" $ property $ \r -> same (X.ref17 (pure r)) (A.ex17 (pure r))

  describe "Lesson 6: recover instead of throwing" $ do
    it "18 a default for a missing value" $ property $ \r -> same (X.ref18 (pure r)) (A.ex18 (pure r))
    it "19 a default after a failure" $ property $ \r -> same (X.ref19 (pure r)) (A.ex19 (pure r))
    it "20 a replacement that depends on the failure" $ property $ \r ->
      same (X.ref20 (pure r)) (A.ex20 (pure r))

  describe "Lesson 7: look at the failure before it is thrown away" $ do
    it "21 log, then throw" $ property $ \r ->
      same (X.ref21 say (pure r)) (A.ex21 say (pure r))
    it "22 log what failed" $ property $ \r ->
      same (X.ref22 say (pure r)) (A.ex22 say (pure r))

  describe "Lesson 8: a whole list of results" $ do
    it "23 every lookup must have found someone" $ forAll smallList $ \r ->
      same (X.ref23 (pure r)) (A.ex23 (pure r))
    it "24 every lookup must have worked" $ forAll smallList $ \r ->
      same (X.ref24 (pure r)) (A.ex24 (pure r))
    it "25 the trap" $ forAll anyGrants $ \r -> same (X.ref25 (pure r)) (A.ex25 (pure r))

  describe "Lesson 9: Validation collects every problem" $ do
    it "26 report every problem" $ forAll (resize 4 arbitrary :: Gen [Validation [String] String]) $ \r ->
      same (X.ref26 (pure r)) (A.ex26 (pure r))
    it "27 the same, with Either" $ forAll (resize 4 arbitrary :: Gen [Either [String] String]) $ \r ->
      same (X.ref27 (pure r)) (A.ex27 (pure r))

  describe "Lesson 10: try one source, then the next" $ do
    it "28 cache, then database" $ property $ \c d ->
      same (X.ref28 (act "cache" c) (act "db" d)) (A.ex28 (act "cache" c) (act "db" d))
    it "29 three sources" $ property $ \a b d ->
      same (X.ref29 (act "c1" a) (act "c2" b) (act "db" d)) (A.ex29 (act "c1" a) (act "c2" b) (act "db" d))
    it "30 combine the problems" $ property $ \a b ->
      same (X.ref30 (act "a" a) (act "b" b)) (A.ex30 (act "a" a) (act "b" b))

  describe "Lesson 11: review B" $ do
    it "31 a positive quota" $ property $ \r -> same (X.ref31 (pure r)) (A.ex31 (pure r))
    it "32 a size limit that names the offender" $
      forAll ((,) <$> choose (-3, 3) <*> arbitrary) $ \(limit, r) ->
        same (X.ref32 limit (pure r)) (A.ex32 limit (pure r))
    it "33 fetch, then log and fall back" $ property $ \p b ->
      same (X.ref33 say (act "primary" p) (act "backup" b)) (A.ex33 say (act "primary" p) (act "backup" b))

  describe "Lesson 12: classify first, then act" $ do
    it "34 must not exist" $ property $ \r -> same (X.ref34 (pure r)) (A.ex34 (pure r))
    it "35 at least one source answered" $ forAll smallList $ \r ->
      same (X.ref35 (pure r)) (A.ex35 (pure r))
    it "36 exactly one, or a stand-in" $ forAll dbUsers $ \r -> same (X.ref36 (pure r)) (A.ex36 (pure r))

  describe "Lesson 13: capstones" $ modifyMaxSuccess (const 1000) $ do
    it "37 find a user" $ forAll ((,,) <$> arbitrary <*> dbUsers <*> dbUsers) $ \(c, r1, r2) ->
      same (X.ref37 (act "cache" c) (act "byId" r1) (act "byEmail" r2))
           (A.ex37 (act "cache" c) (act "byId" r1) (act "byEmail" r2))
    it "38 grant read access" $
      forAll ((,,,) <$> grantsMostly True <*> dbUsers <*> grantsMostly False <*> arbitrary) $
        \(w, t, r, i) ->
          same (X.ref38 say (act "write" w) (act "target" t) (act "read" r) (act "insert" i))
               (A.ex38 say (act "write" w) (act "target" t) (act "read" r) (act "insert" i))
