{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Core.Normalize (normalizeTerm) where

import Deptt.Util.VarSupply (VarSupplyT, fresh, runVarSupplyT)
import Control.Monad.Identity (Identity, runIdentity)
import Deptt.Core.Syntax (Var(..), Term(..), Builtin(..), Scope, abstract, instantiate)
import Data.Text (Text)
import qualified Data.Text as T

newtype Norm a = Norm { runNorm :: VarSupplyT Identity a }
               deriving (Functor, Applicative, Monad)

freshVar :: Norm Text
freshVar = Norm $ do
  i <- fresh
  return $ "__normalize_" <> T.pack (show i)

natElim :: Term -> Term -> Term -> Term -> Term
natElim p b i n = App (App (App (App (Builtin NatElim) p) b) i) n

normalizeBuiltin :: Term -> Term -> Maybe (Norm Term)
normalizeBuiltin (App (App (App (Builtin NatElim) _) nbase) _) (Builtin Zero) = Just (return nbase)
normalizeBuiltin (App (App (App (Builtin NatElim) nprop) nbase) nind) (App (Builtin Succ) k) = Just (normalize (App (App nind k) (natElim nprop nbase nind k)))
normalizeBuiltin (App (App (Builtin Fst) _) _) (App (App (App (App (Builtin Pack) _) _) x) _) = Just (return x)
normalizeBuiltin (App (App (Builtin Snd) _) _) (App (App (App (App (Builtin Pack) _) _) _) y) = Just (return y)
normalizeBuiltin (App (App (App (App (App (Builtin OrElim) _A) _B) _P) left) _right) (App (App (App (Builtin InL) _A2) _B2) x) = Just (normalize (App left x))
normalizeBuiltin (App (App (App (App (App (Builtin OrElim) _A) _B) _P) _left) right) (App (App (App (Builtin InR) _A2) _B2) y) = Just (normalize (App right y))
normalizeBuiltin (App (App (App (App (App (Builtin EqElim) _A) _x) _P) px) _y) (App (App (Builtin Refl) _A2) _x2) = Just (return px)
normalizeBuiltin (App (App (Builtin Proj1) _A1) _B1) (App (App (App (App (Builtin Pair) _A) _B) x) _y) = Just (return x)
normalizeBuiltin (App (App (Builtin Proj2) _A1) _B1) (App (App (App (App (Builtin Pair) _A) _B) _x) y) = Just (return y)
normalizeBuiltin (App (App (Builtin UnitElim) _P) ptt) (Builtin Tt) = Just (return ptt)
normalizeBuiltin _ _ = Nothing

normalize :: Term -> Norm Term
normalize tm@(Var _) = return tm
normalize tm@(Builtin _) = return tm
normalize (Let def _ scope) = normalize =<< (instantiate <$> normalize def <*> pure scope)
normalize (App e1old e2old) = do
  e1norm <- normalize e1old
  e2norm <- normalize e2old
  case normalizeBuiltin e1norm e2norm of
    Just result -> result
    Nothing -> case e1norm of
      Lambda _ scope -> normalize (instantiate e2norm scope)
      _ -> return $ App e1norm e2norm

normalize (Pi ty scope) = Pi <$> normalize ty <*> normalizeScope scope
normalize (Lambda ty scope) = Lambda <$> normalize ty <*> normalizeScope scope

normalizeScope :: Scope -> Norm Scope
normalizeScope scope = do
  f <- freshVar
  let opened = instantiate (Var (Free f)) scope
  normed <- normalize opened
  return $ abstract f normed

normalizeTerm :: Term -> Term
normalizeTerm tm = runIdentity (runVarSupplyT (runNorm (normalize tm)))
