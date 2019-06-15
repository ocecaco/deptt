{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, normalize) where

import Parser (parseNoFail)
import Syntax (Term(..), Binder(..), Builtin(..), scopeApply, shift)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)

type TypeError = String

-- indexed by De Bruijn indices
type Context = [Term]

newtype TC a = TC { runTC :: ExceptT TypeError (ReaderT Context Identity) a }
             deriving (Functor, Applicative, Monad)

getContext :: TC Context
getContext = TC ask

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
        go Refl = parseNoFail "forall A : Type 0, forall x : A, eq A x x"
        go EqElim = parseNoFail "forall A : Type 0, forall x : A, forall P : A -> Type 0, P x -> forall y : A, eq A x y -> P y"

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

-- TODO: normalization necessary for this or is it already done at
-- another point?
checkEqual :: Term -> Term -> TC ()
checkEqual e1 e2
  | normalize e1 == normalize e2 = return ()
  | otherwise = do
      ctxt <- getContext
      typeError "type error"

natElim :: Term -> Term -> Term -> Term -> Term
natElim p b i n = App (App (App (App (Builtin NatElim) p) b) i) n

normalizeNatElim :: Term -> Term -> Term -> Term -> Term
normalizeNatElim _ nbase _ (Builtin Zero) = nbase
normalizeNatElim nprop nbase nind (App (Builtin Succ) k) = normalize (App (App nind k) (natElim nprop nbase nind k))
normalizeNatElim nprop nbase nind n = natElim nprop nbase nind n

-- TODO: what happens during normalization of ill-typed terms? can we
-- be sure that this function is never given an ill-typed term?
normalize :: Term -> Term
normalize tm@(Var _) = tm
normalize tm@(Universe _) = tm
normalize (Let def scope) = normalize (scopeApply scope (normalize def))
normalize (App e1old e2old) = case e1norm of
    Lambda scope -> normalize (scopeApply scope e2norm)
    App (App (App (Builtin NatElim) nprop) nbase) nind -> normalizeNatElim nprop nbase nind e2norm
    -- App (App (App (App (App (Builtin EqElim) _) _) _) h0) _ -> h0 -- the right-hand side (the equality proof itself) is ignored
    _ -> App e1norm e2norm
  where e2norm = normalize e2old
        e1norm = normalize e1old
normalize tm@(Builtin _) = tm

normalize (Pi scope) = Pi (normalizeScope scope)
normalize (Lambda scope) = Lambda (normalizeScope scope)

normalizeScope :: Binder -> Binder
normalizeScope (Binder name ty body) = Binder name (normalize ty) (normalize body)

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
