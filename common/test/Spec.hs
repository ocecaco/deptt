{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Deptt.Core.Syntax
import Deptt.Core.TypeCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ typeCheckerTests ]

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type checker"
  [ makeTestCase b | b <- [minBound..maxBound] ]
  where typeChecks :: Term -> IO ()
        typeChecks t = case typeCheck t of
          Left msg -> assertFailure (T.unpack msg)
          Right _ -> return ()

        makeTestCase builtin = testCase ("Builtin type " <> show builtin) $ typeChecks (builtinType builtin)
