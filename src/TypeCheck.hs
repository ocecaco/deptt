{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeCheck (typeCheck, normalize) where

import Debug.Trace (traceShowM, traceM)
import Syntax (Term(..), Binder(..), scopeApply, shift)
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

inferType :: Term -> TC Term
inferType (Var i) = do
  ty <- lookupType i
  let shifted = shift (i + 1) ty
  traceShowM ty
  traceShowM shifted
  return shifted

inferType (Universe k) = return (Universe (k + 1))
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
  traceM $ "Scope apply: " ++ show scope ++ ", " ++ show e2 ++ ", " ++ show (scopeApply scope e2)
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
      typeError ("type mismatch between " ++ show e1 ++ " and " ++ show e2 ++ " in context " ++ show ctxt)

-- TODO: what happens during normalization of ill-typed terms? can we
-- be sure that this function is never given an ill-typed term?
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
