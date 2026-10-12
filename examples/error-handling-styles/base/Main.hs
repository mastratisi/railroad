{-# LANGUAGE GHC2021, DataKinds #-}
module Main where
import Control.Exception (SomeException, evaluate, try)
import Test.QuickCheck
import H
import Oracle
import StyleCase
import StyleHelper
import StyleRail
import Types
import Unit

db :: World -> Db S
db = mkDb

emails :: Gen [Email]
emails = resize 3 (listOf1 email')

props :: Handlers -> [(String, Property)]
props h =
  [ ("getUser",       property $ \w i -> sh (run (hGetUser h (db w) i)) === oGetUser oracle w i)
  , ("register",      property $ \w e -> sh (run (hRegister h (db w) e)) === oRegister oracle w e)
  , ("checkWrite",    property $ \w u a -> sh (run (hCheckWrite h (db w) u a)) === oCheckWrite oracle w u a)
  , ("findUser",      property $ \w i e -> sh (run (hFindUser h (db w) i e)) === oFindUser oracle w i e)
  , ("grantRead",     property $ \w u a e -> sh (run (hGrantRead h (db w) u a e)) === oGrantRead oracle w u a e)
  , ("uploadNames",   property $ \ns -> sh (run (hUploadNames h ns)) === oUploadNames oracle ns)
  , ("prefsFor",      property $ \w i -> sh (run (hPrefsFor h (db w) i)) === oPrefsFor oracle w i)
  , ("displayName",   property $ \w i -> sh (run (hDisplayName h (db w) i)) === oDisplayName oracle w i)
  , ("useQuota",      property $ \w i -> sh (run (hUseQuota h (db w) i)) === oUseQuota oracle w i)
  , ("canonical",     property $ \w e -> sh (run (hCanonical h (db w) e)) === oCanonical oracle w e)
  , ("shareWithMany", forAll emails $ \es w u a -> sh (run (hShare h (db w) u a es)) === oShare oracle w u a es)
  ]
  where sh :: Show x => x -> String
        sh = show

main :: IO ()
main = mapM_ go [("rail", railH), ("case", caseH), ("helper", helperH)]
  where
    go (style, h) = mapM_ (\(name, p) -> do
        r <- try (quickCheckWithResult stdArgs { maxSuccess = 1500, chatty = False } p)
               :: IO (Either SomeException Result)
        let status = case r of
              Left _ -> "ERR"
              Right Success{} -> "ok"
              Right Failure{theException = Just _} -> "ERR"
              Right Failure{} -> "DRIFT"
              Right _ -> "OTHER"
        putStrLn (style ++ " " ++ name ++ " " ++ status)) (props h)
        >> mapM_ (\(n, f) -> putStrLn (style ++ " unit:" ++ concatMap (\c -> if c == ' ' then "_" else [c]) n ++ " " ++ (if f h then "ok" else "FAIL"))) units
