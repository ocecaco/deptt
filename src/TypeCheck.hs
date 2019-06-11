{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeCheck (typeCheck) where

import Syntax (Term(..), Scope(..), scopeApply)
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
withContext tm (TC act) = TC $ local (\ctxt -> extendContext tm ctxt) act

lookupType :: Int -> TC Term
lookupType idx = TC $ do
  ctxt <- ask
  return (ctxt !! idx)

-- TODO: Maybe introduce newtype to make it harder to forget the
-- withContext wrappers when type checking the body of a scope
inferType :: Term -> TC Term
inferType (Var i) = lookupType i
inferType (Universe k) = return (Universe (k + 1))
inferType (Pi (Scope _ ty body)) = do
  k1 <- inferUniverse ty
  k2 <- withContext ty (inferUniverse body)
  return (Universe (max k1 k2))
inferType (Lambda (Scope _ ty body)) = do
  -- although we do not use the universe of the type, we still have to
  -- make sure it is well-typed itself
  _univ <- inferUniverse ty
  tybody <- withContext ty (inferType body)
  return (Pi (Scope "dummy" ty tybody))

-- here, we check if the type of the argument matches the type
-- expected by the function. the type of the result is then obtained by
-- substituting the argument term into the pi-type.
inferType (App e1 e2) = do
  scope@(Scope _ tyexpect _) <- inferPi e1
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
  | otherwise = typeError "type mismatch"

normalize :: Term -> Term
normalize tm@(Var _) = tm
normalize tm@(Universe _) = tm
normalize (App e1old e2old) = case e1norm of
    Lambda scope -> normalize (scopeApply scope e2norm)
    _ -> App e1norm e2norm
  where e2norm = normalize e2old
        e1norm = normalize e1old

normalize (Pi scope) = Pi (normalizeScope scope)
normalize (Lambda scope) = Lambda (normalizeScope scope)

-- TODO: context isn't extended for the body, is that okay?
normalizeScope :: Scope -> Scope
normalizeScope (Scope name ty body) = Scope name (normalize ty) (normalize body)

inferPi :: Term -> TC Scope
inferPi tm = do
  ty <- inferType tm
  case normalize ty of
    Pi s -> return s
    _ -> typeError "expected pi"

typeCheck :: Term -> Either String Term
typeCheck tm = runIdentity (runReaderT (runExceptT (runTC (inferType tm))) initialContext)
  where initialContext :: Context
        initialContext = []
