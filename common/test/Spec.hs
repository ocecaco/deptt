{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (pi)

import PrettyPrint (prettyPrint)
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
  , prettyPrintTests
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
  , testCase "Nested function without parentheses" $
      parseShouldFail "fun x : Type 0 => x fun x : Type 0 => x"
  , testCase "Nested let without parentheses" $
      parseShouldFail "let x : Type 1 = Type 0 in x let y : Type 1 = Type 0 in x"
  , testCase "Nested pi without parentheses" $
      parseShouldFail "forall A : Type 0, A forall y : A, y"
  ]
  where parseShouldFail s = let parsed = parseTerm s in case parsed of
          Left _ -> return ()
          Right t -> assertFailure $ "Parsing should fail, instead got: " ++ show parsed

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
  , testCase "Natural number elimination" $
      normalize (parseNoFail "natelim (fun p : nat => nat) (succ zero) (fun p : nat => succ) (succ zero)") @?= parseNoFail "succ (succ zero)"
  ]

typeCheckTests :: TestTree
typeCheckTests = testGroup "Type checking"
  [ testCase "Lambda" $
      "fun x : Type 0 => x" `shouldHaveType` "Type 0 -> Type 0"
  , testCase "Universe" $
      "Type 0" `shouldHaveType` "Type 1"
  , testCase "Application" $
      "fun A : Type 0 => fun B : Type 0 => fun f : A -> B => fun x : A => f x" `shouldHaveType` "forall A : Type 0, forall B : Type 0, (A -> B) -> A -> B"
  , testCase "Substitution in let" $
      "let x : Type 2 = Type 1 in (fun y : x => Type 0) Type 0" `shouldHaveType` "Type 1"
  , testCase "Natural number" $
      "succ (succ (succ (succ (succ zero))))" `shouldHaveType` "nat"
  , testCase "Natural number eliminator" $
      "natelim (fun p : nat => nat) (succ zero) (fun p : nat => succ) (succ zero)" `shouldHaveType` "nat"
  , testCase "Equality refl" $
      "fun A : Type 0 => fun x : A => refl A x" `shouldHaveType` "forall A : Type 0, forall x : A, eq A x x"
  , testCase "Equality eliminator" $
      "fun A : Type 0 => fun x : A => fun y : A => fun f : A -> A => fun Heq : eq A (f x) y => fun P : A -> Type 0 => fun Px : P (f x) => eqelim A (f x) P Px y Heq" `shouldHaveType` "forall A : Type 0, forall x : A, forall y : A, forall f : A -> A, eq A (f x) y -> forall P : A -> Type 0, P (f x) -> P y"
  ]
  where tc s = normalize <$> typeCheck s
        shouldHaveType tm ty = tc (parseNoFail tm) @?= (Right $ parseNoFail ty)

prettyPrintTests :: TestTree
prettyPrintTests = testGroup "Pretty printing"
  [ testCase "Application associativity" $
      roundtrip "fun x : Type 0 => x (x x x) (x x) x"
  , testCase "Arrow associativity" $
      roundtrip "fun x : Type 0 => x -> (x -> x -> x) -> (x -> (x -> x)) -> x"
  , testCase "Application and arrow" $
      roundtrip "fun x : Type 0 => x (x -> x) x (x x -> x)"
  , testCase "Pi over arrow" $
      roundtrip "forall x : Type 0, x -> x -> x"
  , testCase "Pi under arrow" $
      roundtrip "fun x : Type 0 => (forall x : Type 0, x) -> x -> x"
  , testCase "Function under app" $
      roundtrip "(fun x : Type 1 => x) Type 0"
  ]
  where roundtrip source = parseTerm (prettyPrint original) @?= Right original
          where original = parseNoFail source
