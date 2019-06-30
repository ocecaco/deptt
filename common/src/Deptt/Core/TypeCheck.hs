{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck (typeCheck, normalize, inferType, inferPi, inferUniverse, builtinType) where

import Deptt.Core.Syntax (Var(..), Term(..), Builtin(..), Scope(..), Name(..), instantiate, scopePrettyName)
import Deptt.Core.Syntax.Builder
import Deptt.Core.TypeCheck.Builtin (builtinType)
import Deptt.Core.TypeCheck.Monad (TC, lookupType, typeError, run, openScope)
import Deptt.Core.TypeCheck.Level (normalizeLevel)
import Deptt.Core.PrettyPrint (prettyPrint)
import Control.Applicative (liftA2)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

inferType :: Term -> TC Term
inferType (Var (Bound _)) = error "type checker encountered bound var"
inferType (Var (Free name)) = lookupType (internalName name)
inferType (Builtin b) = case builtinType b of
  Nothing -> typeError "attempt to take type of typeomega"
  Just ty -> return ty
inferType (Let def ty scope) = do
  _univ <- inferUniverse ty
  tydef <- inferType def
  checkEqual ty tydef def
  inferType (instantiate def scope)
inferType (Pi ty scope) = do
  k1 <- inferUniverse ty
  -- TODO: Is it right that we don't have to close the inferred
  -- universe? Can't it reference the binder from the pi?
  k2 <- openScope ty scope $ \opened _unused_close -> inferUniverse opened
  return $ fromMaybe universeTop $ liftA2 (\x y -> type_ (lmax @@ x @@ y)) k1 k2
inferType (Lambda ty scope) = do
  -- although we do not use the universe of the type, we still have to
  -- make sure it is well-typed itself
  _univ <- inferUniverse ty
  tybody <- openScope ty scope $ \opened close -> close <$> inferType opened
  return (Pi ty tybody)

-- here, we check if the type of the argument matches the type
-- expected by the function. the type of the result is then obtained by
-- substituting the argument term into the pi-type.
inferType (e1 :@ e2) = do
  (tyexpect, scope) <- inferPi e1
  tyarg <- inferType e2
  checkEqual tyexpect tyarg e2
  return (instantiate e2 scope)

inferUniverse :: Term -> TC (Maybe Term)
inferUniverse tm = do
  ty <- inferType tm
  norm <- normalize ty
  case norm of
    Builtin Universe :@ lvl -> return (Just lvl)
    Builtin UniverseTop -> return Nothing
    _ -> typeError $ "type mismatch: expected a type but found " <> prettyQuote tm <> " of type " <> prettyQuote norm <> " which is not a valid type"

prettyQuote :: Term -> Text
prettyQuote tm = "'" <> prettyPrint tm <> "'"

checkEqual :: Term -> Term -> Term -> TC ()
checkEqual expected actual tm = do
  normexpected <- normalize expected
  normactual <- normalize actual
  if normexpected == normactual
    then return ()
    else typeError $ "type mismatch: expected term of type " <> prettyQuote normexpected <> " but found " <> prettyQuote tm <> " which has type " <> prettyQuote normactual

inferPi :: Term -> TC (Term, Scope)
inferPi tm = do
  pity <- inferType tm
  norm <- normalize pity
  case norm of
    Pi ty s -> return (ty, s)
    _ -> typeError $ "type mismatch: cannot apply " <> prettyQuote tm <> " of type " <> prettyQuote norm <> " since it is not a function"

typeCheck :: Term -> Either Text Term
typeCheck tm = run (inferType tm)

normalize :: Term -> TC Term
normalize tm = etaExpand =<< normalizeWithoutEta tm
  where etaExpand :: Term -> TC Term
        etaExpand t@(Lambda _ _) = return t
        etaExpand t = do
          ty <- inferType t
          normty <- normalize ty
          case normty of
            -- it's a pi-typed term that is not a lambda, perform eta expansion
            Pi argty scope -> return $ Lambda argty (ManualScope (scopePrettyName scope) (t :@ Var (Bound 0)))
            _ -> return t

normalizeBuiltin :: Term -> Maybe (TC Term)
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

normalizeWithoutEta :: Term -> TC Term
normalizeWithoutEta tm@(Var _) = return tm
normalizeWithoutEta tm@(Builtin _) = return tm
normalizeWithoutEta (Let def _ scope) = normalize =<< (instantiate <$> normalize def <*> pure scope)
normalizeWithoutEta (e1old :@ e2old) = do
  e1norm <- normalize e1old
  e2norm <- normalize e2old
  case normalizeBuiltin (e1norm :@ e2norm) of
    Just result -> result
    Nothing -> case e1norm of
      Lambda _ scope -> normalize (instantiate e2norm scope)
      _ -> return $ e1norm :@ e2norm

normalizeWithoutEta (Pi ty scope) = Pi <$> normalize ty <*> normalizeScope ty scope
normalizeWithoutEta (Lambda ty scope) = Lambda <$> normalize ty <*> normalizeScope ty scope

normalizeScope :: Term -> Scope -> TC Scope
normalizeScope ty scope =
  openScope ty scope $ \opened close -> do
    normed <- normalize opened
    return $ close normed
