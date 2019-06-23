{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Bidirectional.TypeCheck (infer) where

import Data.Text (Text)
import Deptt.Bidirectional.Syntax (TermC(..), TermI(..), Var(..), instantiateC, instantiateI)
import qualified Data.Text as T
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Trans (lift)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Deptt.Util.VarSupply (VarSupplyT, runVarSupplyT, fresh)
import qualified Deptt.Core.Syntax as C
import qualified Deptt.Core.Normalize as N

-- TODO: might not be necessary to keep the context since we can just
-- store the type in the generated free variables themselves
type Context = Map Text C.Term

newtype TC a = TC { runTC :: ExceptT Text (ReaderT Context (VarSupplyT Identity)) a }
             deriving (Functor, Applicative, Monad)

type TCResult a = (a, C.Term)

withContext :: Text -> C.Term -> TC a -> TC a
withContext name ty (TC act) = TC (local (M.insert name ty) act)

lookupType :: Text -> TC C.Term
lookupType name = TC $ do
  ctxt <- ask
  case M.lookup name ctxt of
    Just ty -> return ty
    Nothing -> error "lookupType: missing name in environment"

typeError :: Text -> TC a
typeError msg = TC (throwError msg)

-- TODO: the (lift . lift) stuff is ugly
freshVar :: TC Text
freshVar = TC $ lift . lift $ do
  i <- fresh
  return $ "__elaborator_" <> T.pack (show i)

inferPi :: TermI -> TC (TCResult (C.Term, C.Scope))
inferPi tm = do
  (ty, trans) <- inferType tm
  case ty of
    C.Pi t s -> return ((t, s), trans)
    _ -> typeError "expected pi"

inferUniverse :: TermI -> TC (TCResult Int)
inferUniverse tm = do
  (ty, trans) <- inferType tm
  case ty of
    C.Universe k -> return (k, trans)
    _ -> typeError "expected pi"

inferType :: TermI -> TC (TCResult C.Term)
inferType (Universe k) = return (C.Universe (k + 1), C.Universe k)
inferType (Var (Bound _)) = error "type checker found bound variable"
inferType (Var (Free n)) = do
  ty <- lookupType n
  return (ty, C.Var (C.Free n))
inferType (Pi ty scope) = do
  (k1, tytrans) <- inferUniverse ty
  name <- freshVar
  let opened = instantiateI (Var (Free name)) scope
  (k2, bodytrans) <- withContext name tytrans $ inferUniverse opened
  return (C.Universe (max k1 k2), C.Pi tytrans (C.abstract name bodytrans))
inferType (App fun arg) = do
  ((piexpect, pibody), transfun) <- inferPi fun
  ((), transarg) <- checkType arg piexpect
  return (C.instantiate transarg pibody, C.App transfun transarg)
inferType (Annotate term ty) = do
  (_univ, transty) <- inferUniverse ty
  ((), transterm) <- checkType term transty
  return (transty, transterm)

inferType (Eq t1 t2) = do
  (ty1, trans1) <- inferType t1
  (ty2, trans2) <- inferType t2
  checkEqual ty1 ty2
  return (C.Universe 0, eq ty1 trans1 trans2)
  where eq :: C.Term -> C.Term -> C.Term -> C.Term
        eq ty x y = ((C.Builtin C.Eq `C.App` ty) `C.App` x) `C.App` y

checkEqual :: C.Term -> C.Term -> TC ()
checkEqual e1 e2
  | norme1 == norme2 = return ()
  | otherwise = typeError "type mismatch"
  where norme1 = N.normalizeTerm e1
        norme2 = N.normalizeTerm e2

checkType :: TermC -> C.Term -> TC (TCResult ())
checkType (Infer term) tyexpect = do
  (tyactual, transterm) <- inferType term
  checkEqual tyexpect tyactual
  return ((), transterm)

checkType (Lambda scope) (C.Pi tyarg pibody) = do
  name <- freshVar
  let openscope = instantiateC (Var (Free name)) scope
  let openpibody = C.instantiate (C.Var (C.Free name)) pibody
  ((), transbody) <- withContext name tyarg (checkType openscope openpibody)
  return ((), C.Lambda tyarg (C.abstract name transbody))

checkType (Lambda _) _ = typeError "non-pi type given to lambda for checking"

-- TODO: This means the translation process inlines all of the local
-- definitions, ideally we would translate a let in the sugary
-- language into a let in the core language
checkType (Let def scope) tyexpect = do
  _ <- inferType def
  ((), transbody) <- checkType (instantiateC def scope) tyexpect
  return ((), transbody)

checkType Refl tyexpect = do
  let normtyexpect = N.normalizeTerm tyexpect
  case normtyexpect of
    ((C.Builtin C.Eq `C.App` ty) `C.App` t1) `C.App` t2 -> do
      checkEqual t1 t2
      return ((), (C.Builtin C.Refl `C.App` ty) `C.App` t1)
    _ -> typeError "refl expects equality type"

infer :: TermI -> Either Text (C.Term, C.Term)
infer tm = runIdentity (runVarSupplyT (runReaderT (runExceptT (runTC $ inferType tm)) initialContext))
  where initialContext :: Context
        initialContext = M.empty
