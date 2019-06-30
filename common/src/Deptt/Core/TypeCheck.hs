{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck (typeCheck, runWithContext, inferType, inferPi, inferUniverse, Context, TC, builtinType) where

import Deptt.Core.Syntax (Var(..), Term(..), Builtin(..), Scope, Name(..), scopePrettyName, abstract, instantiate)
import Deptt.Core.Syntax.Builder (universeTop, lmax, (@@), type_)
import Deptt.Core.TypeCheck.Builtin (builtinType)
import Deptt.Core.Normalize (normalizeTerm)
import Deptt.Core.PrettyPrint (prettyPrint)
import Control.Applicative (liftA2)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Deptt.Util.VarSupply (VarSupplyT, runVarSupplyT, fresh)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)

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
  name <- freshVar
  let namePretty = scopePrettyName scope
  let opened = instantiate (Var (Free (Name name namePretty))) scope
  k2 <- withContext name ty (inferUniverse opened)
  return $ fromMaybe universeTop $ liftA2 (\x y -> type_ (lmax @@ x @@ y)) k1 k2
inferType (Lambda ty scope) = do
  -- although we do not use the universe of the type, we still have to
  -- make sure it is well-typed itself
  _univ <- inferUniverse ty
  name <- freshVar
  let namePretty = scopePrettyName scope
  let opened = instantiate (Var (Free (Name name namePretty))) scope
  tybody <- withContext name ty (inferType opened)
  return (Pi ty (abstract name namePretty tybody))

-- here, we check if the type of the argument matches the type
-- expected by the function. the type of the result is then obtained by
-- substituting the argument term into the pi-type.
inferType (e1 :@ e2) = do
  (tyexpect, scope) <- inferPi e1
  tyarg <- inferType e2
  checkEqual tyexpect tyarg e2
  return (instantiate e2 scope)

typeError :: TypeError -> TC a
typeError msg = TC (throwError msg)

inferUniverse :: Term -> TC (Maybe Term)
inferUniverse tm = do
  ty <- inferType tm
  let norm = normalizeTerm ty
  case norm of
    Builtin Universe :@ lvl -> return (Just lvl)
    Builtin UniverseTop -> return Nothing
    _ -> typeError $ "type mismatch: expected a type but found " <> prettyQuote tm <> " of type " <> prettyQuote norm <> " which is not a valid type"

prettyQuote :: Term -> Text
prettyQuote tm = "'" <> prettyPrint tm <> "'"

checkEqual :: Term -> Term -> Term -> TC ()
checkEqual expected actual tm
  | normexpected == normactual = return ()
  | otherwise = typeError $ "type mismatch: expected term of type " <> prettyQuote normexpected <> " but found " <> prettyQuote tm <> " which has type " <> prettyQuote normactual
  where normexpected = normalizeTerm expected
        normactual = normalizeTerm actual

inferPi :: Term -> TC (Term, Scope)
inferPi tm = do
  pity <- inferType tm
  let norm = normalizeTerm pity
  case norm of
    Pi ty s -> return (ty, s)
    _ -> typeError $ "type mismatch: cannot apply " <> prettyQuote tm <> " of type " <> prettyQuote norm <> " since it is not a function"

typeCheck :: Term -> Either Text Term
typeCheck tm = runWithContext initialContext (inferType tm)
  where initialContext :: Context
        initialContext = M.empty

runWithContext :: Context -> TC a -> Either Text a
runWithContext ctxt act = runIdentity (runVarSupplyT (runReaderT (runExceptT (runTC act)) ctxt))
