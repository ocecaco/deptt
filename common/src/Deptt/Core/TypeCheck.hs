{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck (typeCheck, runWithContext, inferType, inferPi, inferUniverse, Context, TC, builtinType) where

import Deptt.Core.Syntax (Var(..), Term(..), Scope, abstract, instantiate)
import Deptt.Core.TypeCheck.Builtin (builtinType)
import Deptt.Core.Normalize (normalizeTerm)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Deptt.Util.VarSupply (VarSupplyT, runVarSupplyT, fresh)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Map (Map)

type TypeError = Text

type Context = Map Text Term

newtype TC a = TC { runTC :: ExceptT TypeError (ReaderT Context (VarSupplyT Identity)) a }
             deriving (Functor, Applicative, Monad)

freshVar :: TC Text
freshVar = TC $ lift . lift $ do
  i <- fresh
  return $ "__typechecker_" <> T.pack (show i)

withContext :: Text -> Term -> TC a -> TC a
withContext name ty (TC act) = TC (local (M.insert name ty) act)

lookupType :: Text -> TC Term
lookupType name = TC $ do
  ctxt <- ask
  case M.lookup name ctxt of
    Just ty -> return ty
    Nothing -> error "lookupType: missing name in environment"

inferType :: Term -> TC Term
inferType (Var (Bound _)) = error "type checker encountered bound var"
inferType (Var (Free name)) = lookupType name
inferType (Builtin b) = case builtinType b of
  Nothing -> typeError "attempt to take type of Type*"
  Just ty -> return ty
inferType (Let def ty scope) = do
  _univ <- inferUniverse ty
  tydef <- inferType def
  checkEqual ty tydef
  inferType (instantiate def scope)
inferType (Pi ty scope) = do
  k1 <- inferUniverse ty
  name <- freshVar
  let opened = instantiate (Var (Free name)) scope
  k2 <- withContext name ty (inferUniverse opened)
  return undefined -- TODO
inferType (Lambda ty scope) = do
  -- although we do not use the universe of the type, we still have to
  -- make sure it is well-typed itself
  _univ <- inferUniverse ty
  name <- freshVar
  let opened = instantiate (Var (Free name)) scope
  tybody <- withContext name ty (inferType opened)
  return (Pi ty (abstract name tybody))

-- here, we check if the type of the argument matches the type
-- expected by the function. the type of the result is then obtained by
-- substituting the argument term into the pi-type.
inferType (App e1 e2) = do
  (tyexpect, scope) <- inferPi e1
  tyarg <- inferType e2
  checkEqual tyexpect tyarg
  return (instantiate e2 scope)

typeError :: TypeError -> TC a
typeError msg = TC (throwError msg)

inferUniverse :: Term -> TC (Maybe Term)
inferUniverse tm = undefined -- TODO

checkEqual :: Term -> Term -> TC ()
checkEqual e1 e2
  | norme1 == norme2 = return ()
  | otherwise = typeError $ "type mismatch" <> T.pack (show norme1) <> " " <> T.pack (show norme2)
  where norme1 = normalizeTerm e1
        norme2 = normalizeTerm e2

inferPi :: Term -> TC (Term, Scope)
inferPi tm = do
  pity <- inferType tm
  let norm = normalizeTerm pity
  case norm of
    Pi ty s -> return (ty, s)
    _ -> typeError "expected pi"

typeCheck :: Term -> Either Text Term
typeCheck tm = runWithContext initialContext (inferType tm)
  where initialContext :: Context
        initialContext = M.empty

runWithContext :: Context -> TC a -> Either Text a
runWithContext ctxt act = runIdentity (runVarSupplyT (runReaderT (runExceptT (runTC act)) ctxt))
