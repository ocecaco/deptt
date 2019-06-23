{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck (typeCheck, runWithContext, inferType, inferPi, inferUniverse, Context, TC) where

import Deptt.Core.Syntax (Var(..), Term(..), Scope, Builtin(..), abstract, instantiate)
import Deptt.Core.Syntax.Builder
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

builtinType :: Builtin -> Term
builtinType Nat = type_ 0
builtinType Zero = nat
builtinType Succ = nat +-> nat
builtinType NatElim = forall "P" (nat +-> type_ 0) (v "P" @@ zero +-> forall "k" nat (v "P" @@ v "k" +-> v "P" @@ (succ_ @@ v "k")) +-> forall "n" nat (v "P" @@ v "n"))

builtinType Eq = forall "A" (type_ 0) (v "A" +-> v "A" +-> type_ 0)
builtinType Refl = forall "A" (type_ 0) (forall "x" (v "A") (eq @@ v "A" @@ v "x" @@ v "x"))
builtinType EqElim = forall "A" (type_ 0) (forall "x" (v "A") (forall "P" (v "A" +-> type_ 0) (v "P" @@ v "x" +-> forall "y" (v "A") (eq @@ v "A" @@ v "x" @@ v "y" +-> v "P" @@ v "y"))))

builtinType Ex = forall "A" (type_ 0) ((v "A" +-> type_ 0) +-> type_ 0)
builtinType Pack = forall "A" (type_ 0) (forall "P" (v "A" +-> type_ 0) (forall "x" (v "A") (v "P" @@ v "x" +-> ex @@ v"A" @@ v "P")))
builtinType Fst = forall "A" (type_ 0) (forall "P" (v "A" +-> type_ 0) (ex @@ v "A" @@ v "P" +-> v "A"))
builtinType Snd = forall "A" (type_ 0) (forall "P" (v "A" +-> type_ 0) (forall "H" (ex @@ v "A" @@ v "P") (v "P" @@ (fst_ @@ v "A" @@ v "P" @@ v "H"))))

builtinType Or = type_ 0 +-> type_ 0 +-> type_ 0
builtinType InL = forall "A" (type_ 0) (forall "B" (type_ 0) (v "A" +-> or_ @@ v "A" @@ v "B"))
builtinType InR = forall "A" (type_ 0) (forall "B" (type_ 0) (v "B" +-> or_ @@ v "A" @@ v "B"))
builtinType OrElim = forall "A" (type_ 0) (forall "B" (type_ 0) (forall "P" (or_ @@ v "A" @@ v "B" +-> type_ 0) ((forall "a" (v "A") (v "P" @@ (inl @@ v "A" @@ v "B" @@ v "a"))) +-> (forall "b" (v "B") (v "P" @@ (inl @@ v "A" @@ v "B" @@ v "b"))) +-> forall "s" (or_ @@ v "A" @@ v "B") (v "P" @@ v "s"))))

builtinType And = type_ 0 +-> type_ 0 +-> type_ 0
builtinType Pair = forall "A" (type_ 0) (forall "B" (type_ 0) (v "A" +-> v "B" +-> and_ @@ v "A" @@ v "B"))
builtinType Proj1 = forall "A" (type_ 0) (forall "B" (type_ 0) (and_ @@ v "A" @@ v "B" +-> v "A"))
builtinType Proj2 = forall "A" (type_ 0) (forall "B" (type_ 0) (and_ @@ v "A" @@ v "B" +-> v "B"))

builtinType Unit = type_ 0
builtinType Tt = unit
builtinType UnitElim = forall "P" (unit +-> type_ 0) (v "P" @@ tt +-> forall "u" unit (v "P" @@ v "u"))

builtinType Void = type_ 0
builtinType VoidElim = forall "P" (void +-> type_ 0) (forall "e" void (v "P" @@ v "e"))

inferType :: Term -> TC Term
inferType (Var (Bound _)) = error "type checker encountered bound var"
inferType (Var (Free name)) = lookupType name
inferType (Builtin b) = return (builtinType b)
inferType (Universe k) = return (Universe (k + 1))
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
  return (Universe (max k1 k2))
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

inferUniverse :: Term -> TC Int
inferUniverse tm = do
  ty <- inferType tm
  let norm = normalizeTerm ty
  case norm of
    Universe k -> return k
    _ -> typeError "expected universe"

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
