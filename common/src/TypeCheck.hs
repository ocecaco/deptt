{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, normalize) where

import Parser (parseNoFail)
import Syntax (Term(..), Binder(..), Builtin(..), scopeApply, shift)
import Normalize (normalize)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)

type TypeError = String

-- indexed by De Bruijn indices
type Context = [Term]

newtype TC a = TC { runTC :: ExceptT TypeError (ReaderT Context Identity) a }
             deriving (Functor, Applicative, Monad)

extendContext :: Term -> Context -> Context
extendContext tm ctxt = tm : ctxt

withContext :: Term -> TC a -> TC a
withContext tm (TC act) = TC $ local (extendContext tm) act

lookupType :: Int -> TC Term
lookupType idx = TC $ do
  ctxt <- ask
  return (ctxt !! idx)

builtinType :: Builtin -> Term
builtinType = go
  -- TODO: Make this more efficient by caching the results of parsing?
  where go :: Builtin -> Term
        go Nat = parseNoFail "Type 0"
        go Zero = parseNoFail "nat"
        go Succ = parseNoFail "nat -> nat"
        go NatElim = parseNoFail "forall P : nat -> Type 0, P zero -> (forall n : nat, P n -> P (succ n)) -> forall n : nat, P n"
        go Eq = parseNoFail "forall A : Type 0, A -> A -> Type 0"
        go Refl = parseNoFail "forall (A : Type 0) (x : A), eq A x x"
        go EqElim = parseNoFail "forall (A : Type 0) (x : A) (P : A -> Type 0), P x -> forall y : A, eq A x y -> P y"
        go Ex = parseNoFail "forall (A : Type 0), (A -> Type 0) -> Type 0"
        go ExIntro = parseNoFail "forall (A : Type 0) (P : A -> Type 0) (x : A), P x -> ex A P"
        go ExElim = parseNoFail "forall (A : Type 0) (P : A -> Type 0) (Pex : ex A P -> Type 0) (forall (x : A) (p : P x), Pex (exintro A P x p)) -> forall m : ex A P, Pex m"

inferType :: Term -> TC Term
inferType (Var i) = shift (i + 1) <$> lookupType i
inferType (Builtin b) = return (builtinType b)
inferType (Universe k) = return (Universe (k + 1))
inferType (Let def scope@(Binder _ ty _)) = do
  _univ <- inferUniverse ty
  tydef <- inferType def
  checkEqual ty tydef
  inferType (scopeApply scope def)
inferType (Pi (Binder _ ty body)) = do
  k1 <- inferUniverse ty
  k2 <- withContext ty (inferUniverse body)
  return (Universe (max k1 k2))
inferType (Lambda (Binder name ty body)) = do
  -- although we do not use the universe of the type, we still have to
  -- make sure it is well-typed itself
  _univ <- inferUniverse ty
  tybody <- withContext ty (inferType body)
  return (Pi (Binder name ty tybody)) -- TODO: should this name really be a dummy?

-- here, we check if the type of the argument matches the type
-- expected by the function. the type of the result is then obtained by
-- substituting the argument term into the pi-type.
inferType (App e1 e2) = do
  scope@(Binder _ tyexpect _) <- inferPi e1
  tyarg <- inferType e2
  checkEqual tyexpect tyarg
  return (scopeApply scope e2)

typeError :: TypeError -> TC a
typeError msg = TC (throwError msg)

inferUniverse :: Term -> TC Int
inferUniverse tm = do
  ty <- inferType tm
  case normalize ty of
    Universe k -> return k
    _ -> typeError "expected universe"

checkEqual :: Term -> Term -> TC ()
checkEqual e1 e2
  | normalize e1 == normalize e2 = return ()
  | otherwise = typeError "type error"

inferPi :: Term -> TC Binder
inferPi tm = do
  ty <- inferType tm
  case normalize ty of
    Pi s -> return s
    _ -> typeError "expected pi"

typeCheck :: Term -> Either String Term
typeCheck tm = runIdentity (runReaderT (runExceptT (runTC (inferType tm))) initialContext)
  where initialContext :: Context
        initialContext = []
