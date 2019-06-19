{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (pi)

import PrettyPrint (prettyPrint)
import Parser (parseTerm)
import Syntax (Term(..), Binder(..), Builtin(..))
import qualified Parser as P
import TypeCheck (normalize, typeCheck)
import Data.Text (Text)

b :: Builtin -> P.Term
b = P.Builtin

v :: Text -> P.Term
v = P.Var

fun :: Text -> P.Term -> P.Term -> P.Term
fun name ty body = P.Lambda (P.Binder name ty body)

forall :: Text -> P.Term -> P.Term -> P.Term
forall name ty body = P.Pi (P.Binder name ty body)

type_ :: Int -> P.Term
type_ = P.Universe

let_ :: Text -> P.Term -> P.Term -> P.Term -> P.Term
let_ name ty def body = P.Let def (P.Binder name ty body)

infixl 9 @@
infixr 8 +->

(@@) :: P.Term -> P.Term -> P.Term
t1 @@ t2 = P.App t1 t2

(+->) :: P.Term -> P.Term -> P.Term
t1 +-> t2 = P.Pi (P.Binder "_" t1 t2)

tm :: P.Term -> Term
tm t = case P.convertToDeBruijn t of
  Left _ -> error "unexpected scope error in tm"
  Right tB -> tB

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , normalizeTests
  -- , typeCheckTests
  -- , prettyPrintTests
  ]

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "Universe" $
      "Type 0" `parseMatches` type_ 0
  , testCase "Simple lambda" $
      "fun x : Type 0 => x" `parseMatches` fun "x" (type_ 0) (v "x")
  , testCase "Simple pi" $
      "forall x : Type 0, x" `parseMatches` forall "x" (type_ 0) (v "x")
  , testCase "Application" $
      "fun x : Type 0 => fun y : Type 0 => x y" `parseMatches` fun "x" (type_ 0) (fun "y" (type_ 0) (v "x" @@ v "y"))
  , testCase "Arrow type" $
      "Type 0 -> Type 0" `parseMatches` (type_ 0 +-> type_ 0)
  , testCase "Left-associativity of application" $
      "(Type 0) (Type 1) (Type 2)" `parseMatches` ((type_ 0 @@ type_ 1) @@ type_ 2)
  , testCase "Right-associativity of arrow" $
      "Type 0 -> Type 1 -> Type 2" `parseMatches` (type_ 0 +-> (type_ 1 +-> type_ 2))
  , testCase "Let definitions" $
      "let x : Type 1 = Type 0 in let y : Type 1 = x in y" `parseMatches` let_ "x" (type_ 1) (type_ 0) (let_ "y" (type_ 1) (v "x") (v "y"))
  , testCase "Nested function without parentheses" $
      parseShouldFail "fun x : Type 0 => x fun x : Type 0 => x"
  , testCase "Nested let without parentheses" $
      parseShouldFail "let x : Type 1 = Type 0 in x let y : Type 1 = Type 0 in x"
  , testCase "Nested pi without parentheses" $
      parseShouldFail "forall A : Type 0, A forall y : A, y"
  , testCase "Combined arguments single type" $
      "fun A : Type 0 => fun x y : A => A" `parseMatches` fun "A" (type_ 0) (fun "x" (v "A") (fun "y" (v "A") (v "A")))
  , testCase "Combined arguments different types" $
      "fun (A : Type 0) (x y : A) (Heq : eq A x y) => Heq" `parseMatches` fun "A" (type_ 0) (fun "x" (v "A") (fun "y" (v "A") (fun "Heq" (b Eq @@ v "A" @@ v "x" @@ v "y") (v "Heq"))))
  ]
  where parseShouldFail s = let parsed = parseTerm s in case parsed of
          Left _ -> return ()
          Right _ -> assertFailure $ "Parsing should fail, instead got: " ++ show parsed

        parseMatches :: Text -> P.Term -> IO ()
        parseMatches source term = parseTerm source @?= Right (tm term)

normalizeTests :: TestTree
normalizeTests = testGroup "Normalization"
  [ testCase "Simple redex" $
      fun "x" (type_ 1) (v "x") @@ type_ 0 `normalizesTo` type_ 0
  , testCase "Redex under lambda" $
      fun "y" (type_ 0) (fun "x" (type_ 0) (v "x") @@ v "y") `normalizesTo` fun "y" (type_ 0) (v "y")
  , testCase "Redex under pi" $
      forall "x" (type_ 0) (fun "y" (type_ 0) (v "y") @@ v "x") `normalizesTo` forall "x" (type_ 0) (v "x")
  , testCase "Let definition" $
      let_ "y" (type_ 1) (type_ 0) (let_ "x" (type_ 1) (v "y") (v "x")) `normalizesTo` type_ 0
  , testCase "Natural number elimination" $
      (b NatElim @@ fun "p" (b Nat) (b Nat) @@ (b Succ @@ b Zero) @@ fun "p" (b Nat) (b Succ) @@ (b Succ @@ b Zero)) `normalizesTo` (b Succ @@ (b Succ @@ b Zero))
  , testCase "Exists fst elimination" $
      b Fst @@ b Nat @@ fun "x" (b Nat) (b Eq @@ b Nat @@ v "x" @@ v "x") @@ (b ExIntro @@ b Nat @@ fun "x" (b Nat) (b Eq @@ b Nat @@ v "x" @@ v "x") @@ b Zero @@ (b Refl @@ b Nat @@ b Zero)) `normalizesTo` b Zero
  , testCase "Exists snd elimination" $
      (b Snd @@ b Nat @@ fun "x" (b Nat) (b Eq @@ b Nat @@ v "x" @@ v "x") @@ (b ExIntro @@ b Nat @@ fun "x" (b Nat) (b Eq @@ b Nat @@ v "x" @@ v "x") @@ b Zero @@ (b Refl @@ b Nat @@ b Zero))) `normalizesTo` (b Refl @@ b Nat @@ b Zero)
  , testCase "Coproduct elimination (left)" $
      (b OrElim @@ b Nat @@ b Nat @@ fun "p" (b Or @@ b Nat @@ b Nat) (b Nat) @@ b Succ @@ fun "x" (b Nat) (v "x") @@ (b InL @@ b Nat @@ b Nat @@ b Zero)) `normalizesTo` (b Succ @@ b Zero)
  , testCase "Coproduct elimination (right)" $
      (b OrElim @@ b Nat @@ b Nat @@ fun "p" (b Or @@ b Nat @@ b Nat) (b Nat) @@ b Succ @@ fun "x" (b Nat) (v "x") @@ (b InR @@ b Nat @@ b Nat @@ b Zero)) `normalizesTo` b Zero
  , testCase "Equality elimination" $
      (b EqElim @@ b Nat @@ b Zero @@ fun "x" (b Nat) (b Eq @@ b Nat @@ b Zero @@ v "x") @@ (b Refl @@ b Nat @@ b Zero) @@ b Zero @@ (b Refl @@ b Nat @@ b Zero)) `normalizesTo` (b Refl @@ b Nat @@ b Zero)
  ]
  where -- check type preservation
        norm :: Term -> Term
        norm toBeNormalized
          | normalize beforeTy /= normalize afterTy = error "type not preserved during normalization"
          | otherwise = normed
          where getType :: Term -> Term
                getType term = case typeCheck term of
                  Left _ -> error "type checking should not fail in normalization tests"
                  Right ty -> ty

                beforeTy = getType toBeNormalized
                normed = normalize toBeNormalized
                afterTy = getType normed

        normalizesTo :: P.Term -> P.Term -> IO ()
        normalizesTo t1 t2 = norm (tm t1) @?= tm t2

-- typeCheckTests :: TestTree
-- typeCheckTests = testGroup "Type checking"
--   [ testCase "Lambda" $
--       "fun x : Type 0 => x" `hasType` "Type 0 -> Type 0"
--   , testCase "Universe" $
--       "Type 0" `hasType` "Type 1"
--   , testCase "Application" $
--       "fun A : Type 0 => fun B : Type 0 => fun f : A -> B => fun x : A => f x" `hasType` "forall A : Type 0, forall B : Type 0, (A -> B) -> A -> B"
--   , testCase "Substitution in let" $
--       "let x : Type 2 = Type 1 in (fun y : x => Type 0) Type 0" `hasType` "Type 1"
--   , testCase "Natural number" $
--       "succ (succ (succ (succ (succ zero))))" `hasType` "nat"
--   , testCase "Natural number eliminator" $
--       "natelim (fun p : nat => nat) (succ zero) (fun p : nat => succ) (succ zero)" `hasType` "nat"
--   , testCase "Equality refl" $
--       "fun A : Type 0 => fun x : A => refl A x" `hasType` "forall A : Type 0, forall x : A, eq A x x"
--   , testCase "Equality eliminator" $
--       "fun A : Type 0 => fun x : A => fun y : A => fun f : A -> A => fun Heq : eq A (f x) y => fun P : A -> Type 0 => fun Px : P (f x) => eqelim A (f x) P Px y Heq" `hasType` "forall A : Type 0, forall x : A, forall y : A, forall f : A -> A, eq A (f x) y -> forall P : A -> Type 0, P (f x) -> P y"
--   , testCase "Exists intro" $
--       "fun (A : Type 0) (P : A -> Type 0) (x : A) (H : P x) => exintro A P x H" `hasType` "forall (A : Type 0) (P : A -> Type 0) (x : A) (H : P x), ex A P"
--   , testCase "Exists fst" $
--       "fun (A : Type 0) (P : A -> Type 0) (H : ex A P) => fst A P H" `hasType` "forall (A : Type 0) (P : A -> Type 0) (H : ex A P), A"
--   , testCase "Exists snd" $
--       "fun (A : Type 0) (P : A -> Type 0) (H : ex A P) => snd A P H" `hasType` "forall (A : Type 0) (P : A -> Type 0) (H : ex A P), P (fst A P H)"
--   ]
--   where tc s = normalize <$> typeCheck s
--         hasType tm ty = tc (parseNoFail tm) @?= (Right $ parseNoFail ty)

-- prettyPrintTests :: TestTree
-- prettyPrintTests = testGroup "Pretty printing"
--   [ testCase "Application associativity" $
--       roundtrip "fun x : Type 0 => x (x x x) (x x) x"
--   , testCase "Arrow associativity" $
--       roundtrip "fun x : Type 0 => x -> (x -> x -> x) -> (x -> (x -> x)) -> x"
--   , testCase "Application and arrow" $
--       roundtrip "fun x : Type 0 => x (x -> x) x (x x -> x)"
--   , testCase "Pi over arrow" $
--       roundtrip "forall x : Type 0, x -> x -> x"
--   , testCase "Pi under arrow" $
--       roundtrip "fun x : Type 0 => (forall x : Type 0, x) -> x -> x"
--   , testCase "Function under app" $
--       roundtrip "(fun x : Type 1 => x) Type 0"
--   ]
--   where roundtrip source = parseTerm (prettyPrint original) @?= Right original
--           where original = parseNoFail source
