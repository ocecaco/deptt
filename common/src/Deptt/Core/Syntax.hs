module Deptt.Core.Syntax (Var(..), Term(..), Scope, Builtin(..), abstract, instantiate) where

import Data.Text (Text)

data Var = Bound Int -- de Bruijn index
         | Free Text -- free variable by name
         deriving (Eq, Ord, Show)

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Var
          | Pi Term Scope
          | Lambda Term Scope
          | Let Term Term Scope
          | App Term Term

          -- predicative hierarchy of universes
          | Universe Int
          | Builtin Builtin
          deriving (Eq, Ord, Show)

data Scope = Scope Term
              deriving (Ord, Show)

data Builtin = Nat
             | Zero
             | Succ
             | NatElim

             | Eq
             | Refl
             | EqElim

             | Ex
             | Pack
             | Fst
             | Snd

             | Or
             | InL
             | InR
             | OrElim

             | And
             | Pair
             | Proj1
             | Proj2

             | Unit
             | Tt
             | UnitElim

             | Void
             | VoidElim
             deriving (Eq, Ord, Show)

-- names of bound variables are ignored during equality comparison
-- since we are using de Bruijn indices (although the name does get
-- used for pretty printing)
instance Eq Scope where
  Scope body1 == Scope body2 = body1 == body2

-- based on Conor McBride's "I am not a number--I am a free variable"
abstract :: Text -> Term -> Scope
abstract name fullTerm = Scope (go 0 fullTerm)
  where go :: Int -> Term -> Term
        go _ t@(Universe _) = t
        go _ t@(Builtin _) = t
        go _ t@(Var (Bound _)) = t
        go i t@(Var (Free n))
          | name == n = Var (Bound i)
          | otherwise = t
        go i (App t1 t2) = App (go i t1) (go i t2)
        go i (Pi ty scope) = Pi (go i ty) (goScope i scope)
        go i (Lambda ty scope) = Lambda (go i ty) (goScope i scope)
        go i (Let def ty scope) = Let (go i def) (go i ty) (goScope i scope)

        goScope :: Int -> Scope -> Scope
        goScope i (Scope body) = Scope (go (i + 1) body)

-- TODO: add debug check to ensure that there are no dangling de
-- bruijn variables in the term that is being substituted?
instantiate :: Term -> Scope -> Term
instantiate sub (Scope body) = go 0 body
  where go :: Int -> Term -> Term
        go _ t@(Universe _) = t
        go _ t@(Builtin _) = t
        go _ t@(Var (Free _)) = t
        go i t@(Var (Bound k))
          | i == k = sub
          | otherwise = t
        go i (App t1 t2) = App (go i t1) (go i t2)
        go i (Lambda ty scope) = Lambda (go i ty) (goScope i scope)
        go i (Pi ty scope) = Pi (go i ty) (goScope i scope)
        go i (Let def ty scope) = Let (go i def) (go i ty) (goScope i scope)

        goScope :: Int -> Scope -> Scope
        goScope i (Scope inner) = Scope (go (i + 1) inner)
