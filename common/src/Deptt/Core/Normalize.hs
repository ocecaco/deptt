{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Core.Normalize (normalizeTerm) where

import Deptt.Util.VarSupply (VarSupplyT, fresh, runVarSupplyT)
import Control.Monad.Identity (Identity, runIdentity)
import Deptt.Core.Syntax (Var(..), Term(..), Builtin(..), Scope, abstract, instantiate)
import Deptt.Core.Syntax.Builder
import Deptt.Core.Normalize.Level (normalizeLevel)
import Data.Text (Text)
import qualified Data.Text as T

newtype Norm a = Norm { runNorm :: VarSupplyT Identity a }
               deriving (Functor, Applicative, Monad)

freshVar :: Norm Text
freshVar = Norm $ do
  i <- fresh
  return $ "__normalize_" <> T.pack (show i)

normalizeBuiltin :: Term -> Maybe (Norm Term)
normalizeBuiltin (Builtin NatElim :@ _ :@ nbase :@ _ :@ Builtin Zero) = Just (return nbase)
normalizeBuiltin (Builtin NatElim :@ nprop :@ nbase :@ nind :@ (Builtin Succ :@ k)) = Just (normalize (nind :@ k :@ (natelim @@ nprop @@ nbase @@ nind @@ k)))
normalizeBuiltin (Builtin Fst :@ _ :@ _ :@ (Builtin Pack :@ _ :@ _ :@ x :@ _)) = Just (return x)
normalizeBuiltin (Builtin Snd :@ _ :@ _ :@ (Builtin Pack :@ _ :@ _ :@ _ :@ y)) = Just (return y)
normalizeBuiltin (Builtin OrElim :@ _A :@ _B :@ _P :@ left :@ _right :@ (Builtin InL :@ _A2 :@ _B2 :@ x)) = Just (normalize (left :@ x))
normalizeBuiltin (Builtin OrElim :@ _A :@ _B :@ _P :@ _left :@ right :@ (Builtin InR :@ _A2 :@ _B2 :@ y)) = Just (normalize (right :@ y))
normalizeBuiltin (Builtin EqElim :@ _A :@ _x :@ _P :@ px :@ _y :@ (Builtin Refl :@ _A2 :@ _x2)) = Just (return px)
normalizeBuiltin (Builtin Proj1 :@ _A1 :@ _B1 :@ (Builtin Pair :@ _A :@ _B :@ x :@ _y)) = Just (return x)
normalizeBuiltin (Builtin Proj2 :@ _A1 :@ _B1 :@ (Builtin Pair :@ _A :@ _B :@ _x :@ y)) = Just (return y)
normalizeBuiltin (Builtin UnitElim :@ _P :@ ptt :@ Builtin Tt) = Just (return ptt)
normalizeBuiltin t@(Builtin LevelSucc :@ _) = Just . return $ normalizeLevel t
normalizeBuiltin t@(Builtin LevelMax :@ _ :@ _) = Just . return $ normalizeLevel t
normalizeBuiltin _ = Nothing

normalize :: Term -> Norm Term
normalize tm@(Var _) = return tm
normalize tm@(Builtin _) = return tm
normalize (Let def _ scope) = normalize =<< (instantiate <$> normalize def <*> pure scope)
normalize (e1old :@ e2old) = do
  e1norm <- normalize e1old
  e2norm <- normalize e2old
  case normalizeBuiltin (e1norm :@ e2norm) of
    Just result -> result
    Nothing -> case e1norm of
      Lambda _ scope -> normalize (instantiate e2norm scope)
      _ -> return $ e1norm :@ e2norm

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
