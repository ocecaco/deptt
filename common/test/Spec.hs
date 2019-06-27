{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Deptt.Core.Syntax
import Deptt.Core.Syntax.Builder
import Deptt.Core.TypeCheck
import Deptt.Core.Normalize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ typeCheckerTests
  , normalizationTests ]

typeCheckerTests :: TestTree
typeCheckerTests = testGroup "Type checker"
  [ makeTestCase b | b <- [minBound..maxBound] ]
  where typeChecks :: Term -> IO ()
        typeChecks t = case typeCheck t of
          Left msg -> assertFailure (T.unpack msg)
          Right _ -> return ()

        makeTestCase builtin = testCase ("Builtin type " <> show builtin) $
          case builtinType builtin of
            Nothing -> return ()
            Just ty -> typeChecks ty

normalizationTests :: TestTree
normalizationTests = testGroup "Normalization"
  [ testCase "natelim" $
      (natelim @@ lzero @@ fun "p" nat nat @@ (succ_ @@ (succ_ @@ zero)) @@ fun "p" nat succ_ @@ (succ_ @@ (succ_ @@ zero))) `normalizesTo` (succ_ @@ (succ_ @@ (succ_ @@ (succ_ @@ zero))))
  , testCase "fst" $
      (fst_ @@ lzero @@ nat @@ fun "p" nat nat @@ (pack @@ lzero @@ nat @@ fun "p" nat nat @@ zero @@ (succ_ @@ zero))) `normalizesTo` zero
  , testCase "snd" $
      (snd_ @@ lzero @@ nat @@ fun "p" nat nat @@ (pack @@ lzero @@ nat @@ fun "p" nat nat @@ zero @@ (succ_ @@ zero))) `normalizesTo` (succ_ @@ zero)
  , testCase "orelim left" $
      (orelim @@ lzero @@ nat @@ nat @@ fun "p" (or_ @@ lzero @@ nat @@ nat) nat @@ fun "x" nat (v "x") @@ fun "x" nat (succ_ @@ v "x") @@ (inl @@ lzero @@ nat @@ nat @@ zero)) `normalizesTo` zero
  ]
  where normalizesTo :: Term -> Term -> IO ()
        normalizesTo source target = case typeCheck source of
          Left _ -> assertFailure "source is ill-typed"
          Right sourceTy -> case typeCheck target of
            Left _ -> assertFailure "target is ill-typed"
            Right targetTy -> do
              let sourceTyNorm = normalizeTerm sourceTy
              let targetTyNorm = normalizeTerm targetTy
              assertEqual "source and target types must match" targetTyNorm sourceTyNorm
              assertEqual "source should normalize to target" target (normalizeTerm source)
