{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Deptt.Bidirectional.Syntax (inferType, instantiateC, instantiateI) where

import Data.Text (Text)
import Control.Monad.Identity (Identity)
import qualified Deptt.Core.Syntax as C
import qualified Deptt.Core.Normalize as N

data Var = Bound Int
         | Free Text
         deriving (Eq, Ord, Show)

data TermC = Lambda (Scope TermC)
           | Let TermI (Scope TermC)
           | Infer TermI
           | Refl
           deriving (Eq, Ord, Show)

data TermI = Var Var
           | Universe Int
           | Pi TermI (Scope TermI)
           | App TermI TermC
           | Annotate TermC TermI

           | Eq TermI TermI
           deriving (Eq, Ord, Show)

newtype Scope a = Scope a
                deriving (Eq, Ord, Show)

newtype TC a = TC (Identity a)
             deriving (Functor, Applicative, Monad)

type TCResult a = (a, C.Term)

instantiateC :: TermI -> Scope TermC -> TermC
instantiateC sub (Scope body) = instantiateC' sub 0 body

instantiateI :: TermI -> Scope TermI -> TermI
instantiateI sub (Scope body) = instantiateI' sub 0 body

instantiateI' :: TermI -> Int -> TermI -> TermI
instantiateI' _ _ t@(Var (Free _)) = t
instantiateI' sub i t@(Var (Bound k))
    | i == k = sub
    | otherwise = t
instantiateI' _ _ t@(Universe _) = t
instantiateI' sub i (Pi ty scope) = Pi (instantiateI' sub i ty) (instantiateScopeI' sub i scope)
instantiateI' sub i (App t1 t2) = App (instantiateI' sub i t1) (instantiateC' sub i t2)
instantiateI' sub i (Annotate tm ty) = Annotate (instantiateC' sub i tm) (instantiateI' sub i ty)
instantiateI' sub i (Eq t1 t2) = Eq (instantiateI' sub i t1) (instantiateI' sub i t2)

instantiateC' :: TermI -> Int -> TermC -> TermC
instantiateC' sub i (Lambda scope) = Lambda (instantiateScopeC' sub i scope)
instantiateC' sub i (Let def scope) = Let (instantiateI' sub i def) (instantiateScopeC' sub i scope)
instantiateC' sub i (Infer t) = Infer (instantiateI' sub i t)
instantiateC' _ _ t@Refl = t

instantiateScopeI' :: TermI -> Int -> Scope TermI -> Scope TermI
instantiateScopeI' sub i (Scope t) = Scope (instantiateI' sub (i + 1) t)

instantiateScopeC' :: TermI -> Int -> Scope TermC -> Scope TermC
instantiateScopeC' sub i (Scope t) = Scope (instantiateC' sub (i + 1) t)

withContext :: Text -> C.Term -> TC a -> TC a
withContext = undefined

getVarType :: Text -> TC C.Term
getVarType = undefined

typeError :: Text -> TC a
typeError = undefined

freshVar :: TC Text
freshVar = undefined

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
  ty <- getVarType n
  return (ty, C.Var (C.Free n))
inferType (Pi ty scope) = do
  (k1, tytrans) <- inferUniverse ty
  name <- freshVar
  let opened = instantiateI (Var (Free name)) scope
  (k2, bodytrans) <- inferUniverse opened
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
