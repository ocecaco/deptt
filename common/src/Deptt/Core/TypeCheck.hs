{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck (typeCheck, inferType, inferPi, inferUniverse, builtinType) where

import Deptt.Core.Syntax (Var(..), Term(..), Builtin(..), Scope(ManualScope), Name(..), instantiate, isUnusedScope)
import Deptt.Core.Syntax.Builder (universeTop, lmax, (@@), type_)
import Deptt.Core.TypeCheck.Builtin (builtinType)
import Deptt.Core.TypeCheck.Monad (TC, lookupType, typeError, run, openScope)
import Deptt.Core.Normalize (normalize)
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
  k2 <- openScope ty scope $ \opened close -> do
    result <- inferUniverse opened
    let maybeScope = close <$> result
    case maybeScope of
      Nothing -> return Nothing
      Just sc@(ManualScope _ body) ->
        if isUnusedScope sc
        then return $ Just body
        else return Nothing
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
