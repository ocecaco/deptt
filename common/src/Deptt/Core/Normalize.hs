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
normalizeBuiltin (Builtin NatElim :@ _lvl :@ _ :@ nbase :@ _ :@ Builtin Zero) = Just (return nbase)
normalizeBuiltin (Builtin NatElim :@ lvl :@ nprop :@ nbase :@ nind :@ (Builtin Succ :@ k)) = Just (normalize (nind :@ k :@ (natelim @@ lvl @@ nprop @@ nbase @@ nind @@ k)))
normalizeBuiltin (Builtin Fst :@ _lvl1 :@ _lvl2 :@ _ :@ _ :@ (Builtin Pack :@ _lvl3 :@ _lvl4 :@ _ :@ _ :@ x :@ _)) = Just (return x)
normalizeBuiltin (Builtin Snd :@ _lvl1 :@ _lvl2 :@ _ :@ _ :@ (Builtin Pack :@ _lvl3 :@ _lvl4 :@ _ :@ _ :@ _ :@ y)) = Just (return y)
normalizeBuiltin (Builtin OrElim :@ _lvl1 :@ _lvl2 :@ _lvl3 :@ _A :@ _B :@ _P :@ left :@ _right :@ (Builtin InL :@ _lvl4 :@ _lvl5 :@ _A2 :@ _B2 :@ x)) = Just (normalize (left :@ x))
normalizeBuiltin (Builtin OrElim :@ _ :@ _ :@ _ :@ _A :@ _B :@ _P :@ _left :@ right :@ (Builtin InR :@ _ :@ _ :@ _A2 :@ _B2 :@ y)) = Just (normalize (right :@ y))
normalizeBuiltin (Builtin EqElim :@ _lvl :@ _ :@ _A :@ _x :@ _P :@ px :@ _y :@ (Builtin Refl :@ _lvl2 :@ _A2 :@ _x2)) = Just (return px)
normalizeBuiltin (Builtin Proj1 :@ _lvl :@ _ :@ _A1 :@ _B1 :@ (Builtin Pair :@ _lvl2 :@ _ :@ _A :@ _B :@ x :@ _y)) = Just (return x)
normalizeBuiltin (Builtin Proj2 :@ _lvl :@ _ :@ _A1 :@ _B1 :@ (Builtin Pair :@ _lvl2 :@ _ :@ _A :@ _B :@ _x :@ y)) = Just (return y)
normalizeBuiltin (Builtin UnitElim :@ _lvl :@ _P :@ ptt :@ Builtin Tt) = Just (return ptt)
normalizeBuiltin (Builtin ListElim :@ _lvl1 :@ _lvl2 :@ _A1 :@ _P :@ base :@ _ind :@ (Builtin Nil :@ _lvl3 :@ _A2)) = Just (return base)
normalizeBuiltin (Builtin ListElim :@ lvl1 :@ lvl2 :@ tyA1 :@ tyP :@ base :@ ind :@ (Builtin Cons :@ _lvl3 :@ _A2 :@ h :@ hs)) = Just (normalize (ind @@ h @@ hs @@ (listelim @@ lvl1 @@ lvl2 @@ tyA1 @@ tyP @@ base @@ ind @@ hs)))
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
