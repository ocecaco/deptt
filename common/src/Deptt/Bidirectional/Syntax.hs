module Deptt.Bidirectional.Syntax (TermC(..), TermI(..), Var(..), Scope, instantiateC, instantiateI) where

import Data.Text (Text)

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
