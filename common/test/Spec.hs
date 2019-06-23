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
  [ testCase "Builtin type nat" $ typeChecks (builtinType Nat)
  , testCase "Builtin type zero" $ typeChecks (builtinType Zero)
  , testCase "Builtin type succ" $ typeChecks (builtinType Succ)
  , testCase "Builtin type natelim" $ typeChecks (builtinType NatElim)
  , testCase "Builtin type eq" $ typeChecks (builtinType Eq)
  , testCase "Builtin type refl" $ typeChecks (builtinType Refl)
  , testCase "Builtin type eqelim" $ typeChecks (builtinType EqElim)
  , testCase "Builtin type ex" $ typeChecks (builtinType Ex)
  , testCase "Builtin type pack" $ typeChecks (builtinType Pack)
  , testCase "Builtin type fst" $ typeChecks (builtinType Fst)
  , testCase "Builtin type snd" $ typeChecks (builtinType Snd)
  , testCase "Builtin type or" $ typeChecks (builtinType Or)
  , testCase "Builtin type inl" $ typeChecks (builtinType InL)
  , testCase "Builtin type inr" $ typeChecks (builtinType InR)
  , testCase "Builtin type orelim" $ typeChecks (builtinType OrElim)
  , testCase "Builtin type and" $ typeChecks (builtinType And)
  , testCase "Builtin type pair" $ typeChecks (builtinType Pair)
  , testCase "Builtin type proj1" $ typeChecks (builtinType Proj1)
  , testCase "Builtin type proj2" $ typeChecks (builtinType Proj2)
  , testCase "Builtin type unit" $ typeChecks (builtinType Unit)
  , testCase "Builtin type tt" $ typeChecks (builtinType Tt)
  , testCase "Builtin type unitelim" $ typeChecks (builtinType UnitElim)
  , testCase "Builtin type void" $ typeChecks (builtinType Void)
  , testCase "Builtin type voidelim" $ typeChecks (builtinType VoidElim)
  ]
  where typeChecks :: Term -> IO ()
        typeChecks t = case typeCheck t of
          Left msg -> assertFailure (T.unpack msg)
          Right _ -> return ()
