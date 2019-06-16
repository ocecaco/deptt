{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (pi)

import Parser (parseTerm, parseNoFail)
import Syntax (Term(..), Binder(..))
import TypeCheck (normalize, typeCheck)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , normalizeTests
  , typeCheckTests
  ]

lambda :: Term -> Term -> Term
lambda ty body = Lambda (Binder "_" ty body)

pi :: Term -> Term -> Term
pi ty body = Pi (Binder "var" ty body)

let_ :: Term -> Term -> Term -> Term
let_ ty def body = Let def (Binder "var" ty body)

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "Universe" $
      parseTerm "Type 0" @?= Right (Universe 0)
  , testCase "Simple lambda" $
      parseTerm "fun x : Type 0 => x" @?= Right (lambda (Universe 0) (Var 0))
  , testCase "Simple pi" $
      parseTerm "forall x : Type 0, x" @?= Right (pi (Universe 0) (Var 0))
  , testCase "Application" $
      parseTerm "fun x : Type 0 => fun y : Type 0 => x y" @?= Right (lambda (Universe 0) (lambda (Universe 0) (App (Var 1) (Var 0))))
  , testCase "Arrow type" $
      parseTerm "Type 0 -> Type 0" @?= Right (pi (Universe 0) (Universe 0))
  , testCase "Left-associativity of application" $
      parseTerm "(Type 0) (Type 1) (Type 2)" @?= Right (App (App (Universe 0) (Universe 1)) (Universe 2))
  , testCase "Right-associativity of arrow" $
      parseTerm "Type 0 -> Type 1 -> Type 2" @?= Right (pi (Universe 0) (pi (Universe 1) (Universe 2)))
  , testCase "Let definitions" $
      parseTerm "let x : Type 1 = Type 0 in let y : Type 1 = x in y" @?= Right (let_ (Universe 1) (Universe 0) (let_ (Universe 1) (Var 0) (Var 0)))
  ]

normalizeTests :: TestTree
normalizeTests = testGroup "Normalization"
  [ testCase "Simple redex" $
      normalize (parseNoFail "(fun x : Type 1 => x) (Type 0)") @?= parseNoFail "Type 0"
  , testCase "Redex under lambda" $
      normalize (parseNoFail "fun y : Type 0 => (fun x : Type 0 => x) y") @?= parseNoFail "fun y : Type 0 => y"
  , testCase "Redex under pi" $
      normalize (parseNoFail "forall x : Type 0, (fun y : Type 0 => y) x") @?= parseNoFail "forall x : Type 0, x"
  , testCase "Let definition" $
      normalize (parseNoFail "let y : Type 1 = Type 0 in (let x : Type 1 = y in x)") @?= parseNoFail "Type 0"
  ]

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type checking"
  [ testCase "Lambda" $
      tc (parseNoFail "fun x : Type 0 => x") @?= (Right $ parseNoFail "Type 0 -> Type 0")
  , testCase "Universe" $
      tc (parseNoFail "Type 0") @?= (Right $ parseNoFail "Type 1")
  , testCase "Application" $
      tc (parseNoFail "fun A : Type 0 => fun B : Type 0 => fun f : A -> B => fun x : A => f x") @?= (Right $ parseNoFail "forall A : Type 0, forall B : Type 0, (A -> B) -> A -> B")
  , testCase "Substitution in let" $
      tc (parseNoFail "let x : Type 2 = Type 1 in (fun y : x => Type 0) Type 0") @?= (Right $ parseNoFail "Type 1")
  ]
  where tc s = normalize <$> typeCheck s
