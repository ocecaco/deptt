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
  , testCase "Combined arguments single type" $
      parseTerm "fun A : Type 0 => fun x y : A => A" @?= (Right $ parseNoFail "fun A : Type 0 => fun x : A => fun y : A => A")
  , testCase "Combined arguments different types" $
      parseTerm "fun (A : Type 0) (x y : A) (Heq : eq A x y) => Heq" @?= (Right $ parseNoFail "fun A : Type 0 => fun x : A => fun y : A => fun Heq : eq A x y => Heq")
  ]
  where parseShouldFail s = let parsed = parseTerm s in case parsed of
          Left _ -> return ()
          Right _ -> assertFailure $ "Parsing should fail, instead got: " ++ show parsed

normalizeTests :: TestTree
normalizeTests = testGroup "Normalization"
  [ testCase "Simple redex" $
      norm (parseNoFail "(fun x : Type 1 => x) (Type 0)") @?= parseNoFail "Type 0"
  , testCase "Redex under lambda" $
      norm (parseNoFail "fun y : Type 0 => (fun x : Type 0 => x) y") @?= parseNoFail "fun y : Type 0 => y"
  , testCase "Redex under pi" $
      norm (parseNoFail "forall x : Type 0, (fun y : Type 0 => y) x") @?= parseNoFail "forall x : Type 0, x"
  , testCase "Let definition" $
      norm (parseNoFail "let y : Type 1 = Type 0 in (let x : Type 1 = y in x)") @?= parseNoFail "Type 0"
  , testCase "Natural number elimination" $
      norm (parseNoFail "natelim (fun p : nat => nat) (succ zero) (fun p : nat => succ) (succ zero)") @?= parseNoFail "succ (succ zero)"
  , testCase "Exists fst elimination" $
      norm (parseNoFail "fst nat (fun x : nat => eq nat x x) ((exintro nat (fun x : nat => eq nat x x) zero (refl nat zero)))") @?= parseNoFail "zero"
  , testCase "Exists snd elimination" $
      norm (parseNoFail "snd nat (fun x : nat => eq nat x x) ((exintro nat (fun x : nat => eq nat x x) zero (refl nat zero)))") @?= parseNoFail "refl nat zero"
  , testCase "Coproduct elimination (left)" $
      norm (parseNoFail "orelim nat nat (fun p : or nat nat => nat) succ (fun x : nat => x) (inl nat nat zero)") @?= parseNoFail "succ zero"
  , testCase "Coproduct elimination (right)" $
      norm (parseNoFail "orelim nat nat (fun p : or nat nat => nat) succ (fun x : nat => x) (inr nat nat zero)") @?= parseNoFail "zero"
  , testCase "Equality elimination" $
      norm (parseNoFail "eqelim nat zero (fun x : nat => eq nat zero x) (refl nat zero) zero (refl nat zero)") @?= parseNoFail "refl nat zero"
  ]
  where -- check type preservation
        norm :: Term -> Term
        norm toBeNormalized
          | normalize beforeTy /= normalize afterTy = error "type not preserved during normalization"
          | otherwise = normed
          where getType :: Term -> Term
                getType tm = case typeCheck tm of
                  Left _ -> error "type checking should not fail in normalization tests"
                  Right ty -> ty

                beforeTy = getType toBeNormalized
                normed = normalize toBeNormalized
                afterTy = getType normed

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
  , testCase "Exists intro" $
      "fun (A : Type 0) (P : A -> Type 0) (x : A) (H : P x) => exintro A P x H" `shouldHaveType` "forall (A : Type 0) (P : A -> Type 0) (x : A) (H : P x), ex A P"
  , testCase "Exists fst" $
      "fun (A : Type 0) (P : A -> Type 0) (H : ex A P) => fst A P H" `shouldHaveType` "forall (A : Type 0) (P : A -> Type 0) (H : ex A P), A"
  , testCase "Exists snd" $
      "fun (A : Type 0) (P : A -> Type 0) (H : ex A P) => snd A P H" `shouldHaveType` "forall (A : Type 0) (P : A -> Type 0) (H : ex A P), P (fst A P H)"
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
