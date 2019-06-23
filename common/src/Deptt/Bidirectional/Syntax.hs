module Deptt.Bidirectional.Syntax (TermC(..), TermI(..), Var(..), Scope(..), instantiateC, instantiateI, abstractC, abstractI) where

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

abstractC :: Text -> TermC -> Scope TermC
abstractC name t = Scope (abstractC' name 0 t)

abstractI :: Text -> TermI -> Scope TermI
abstractI name t = Scope (abstractI' name 0 t)

abstractC' :: Text -> Int -> TermC -> TermC
abstractC' name i (Lambda scope) = Lambda (abstractScopeC' name i scope)
abstractC' name i (Let def scope) = Let (abstractI' name i def) (abstractScopeC' name i scope)
abstractC' name i (Infer t) = Infer (abstractI' name i t)
abstractC' _ _ t@Refl = t

abstractI' :: Text -> Int -> TermI -> TermI
abstractI' _ _ t@(Universe _) = t
abstractI' _ _ t@(Var (Bound _)) = t
abstractI' name i t@(Var (Free n))
  | name == n = Var (Bound i)
  | otherwise = t
abstractI' name i (Pi ty scope) = Pi (abstractI' name i ty) (abstractScopeI' name i scope)
abstractI' name i (App t1 t2) = App (abstractI' name i t1) (abstractC' name i t2)
abstractI' name i (Annotate tm ty) = Annotate (abstractC' name i tm) (abstractI' name i ty)
abstractI' name i (Eq t1 t2) = Eq (abstractI' name i t1) (abstractI' name i t2)

abstractScopeC' :: Text -> Int -> Scope TermC -> Scope TermC
abstractScopeC' name i (Scope t) = Scope (abstractC' name (i + 1) t)

abstractScopeI' :: Text -> Int -> Scope TermI -> Scope TermI
abstractScopeI' name i (Scope t) = Scope (abstractI' name (i + 1) t)
