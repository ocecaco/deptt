import Test.Tasty
import Test.Tasty.HUnit

import Prelude hiding (pi)

import Parser (parseTerm)
import Syntax (Term(..), Binder(..))
import TypeCheck (normalize)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
  [ parserTests
  , normalizeTests
  ]

lambda :: Term -> Term -> Term
lambda ty body = Lambda (Binder (Just "var") ty body)

pi :: Term -> Term -> Term
pi ty body = Pi (Binder (Just "var") ty body)

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
  ]

normalizeTests :: TestTree
normalizeTests = testGroup "Normalization"
  [ testCase "Simple redex" $
      normalize (parse "(fun x : Type 1 => x) (Type 0)") @?= Universe 0
  , testCase "Redex under lambda" $
      normalize (parse "fun y : Type 0 => (fun x : Type 0 => x) y") @?= lambda (Universe 0) (Var 0)
  , testCase "Redex under pi" $
      normalize (parse "forall x : Type 0, (fun y : Type 0 => y) x") @?= pi (Universe 0) (Var 0)
  ]
  where parse :: String -> Term
        parse str = case parseTerm str of
                      Left _ -> error "parsing is assumed not to fail during this test"
                      Right t -> t
