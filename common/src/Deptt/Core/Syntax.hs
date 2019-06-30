module Deptt.Core.Syntax (Var(..), Term(..), Scope(..), Builtin(..), Name(..), PrettyName(..), abstract, instantiate, scopePrettyName) where

import Data.Text (Text)

newtype PrettyName = PrettyName { unwrapPrettyName :: Text }
                   deriving (Ord)

-- don't take into account the pretty-printing names when comparing
-- terms for equality
instance Eq PrettyName where
  _ == _ = True

data Name = Name { internalName :: Text, prettyName :: PrettyName }
          deriving (Eq, Ord)

data Var = Bound Int -- de Bruijn index
         | Free Name -- free variable by name
         deriving (Eq, Ord)

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Var
          | Pi Term Scope
          | Lambda Term Scope
          | Let Term Term Scope
          | Term :@ Term

          | Builtin Builtin
          deriving (Eq, Ord)

infixl 8 :@

data Scope = ManualScope PrettyName Term
              deriving (Eq, Ord)

data Builtin = Nat
             | Zero
             | Succ
             | NatElim

             | List
             | Nil
             | Cons
             | ListElim

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

             | Level
             | LevelZero
             | LevelSucc
             | LevelMax

             | UniverseTop
             | Universe
             deriving (Eq, Ord, Enum, Bounded)

-- based on Conor McBride's "I am not a number--I am a free variable"
abstract :: Text -> PrettyName -> Term -> Scope
abstract name namePretty fullTerm = ManualScope namePretty (go 0 fullTerm)
  where go :: Int -> Term -> Term
        go _ t@(Builtin _) = t
        go _ t@(Var (Bound _)) = t
        go i t@(Var (Free n))
          | name == internalName n = Var (Bound i)
          | otherwise = t
        go i (t1 :@ t2) = go i t1 :@ go i t2
        go i (Pi ty scope) = Pi (go i ty) (goScope i scope)
        go i (Lambda ty scope) = Lambda (go i ty) (goScope i scope)
        go i (Let def ty scope) = Let (go i def) (go i ty) (goScope i scope)

        goScope :: Int -> Scope -> Scope
        goScope i (ManualScope pretty body) = ManualScope pretty (go (i + 1) body)

-- TODO: add debug check to ensure that there are no dangling de
-- bruijn variables in the term that is being substituted?
instantiate :: Term -> Scope -> Term
instantiate sub (ManualScope _ body) = go 0 body
  where go :: Int -> Term -> Term
        go _ t@(Builtin _) = t
        go _ t@(Var (Free _)) = t
        go i t@(Var (Bound k))
          | i == k = sub
          | otherwise = t
        go i (t1 :@ t2) = go i t1 :@ go i t2
        go i (Lambda ty scope) = Lambda (go i ty) (goScope i scope)
        go i (Pi ty scope) = Pi (go i ty) (goScope i scope)
        go i (Let def ty scope) = Let (go i def) (go i ty) (goScope i scope)

        goScope :: Int -> Scope -> Scope
        goScope i (ManualScope pretty inner) = ManualScope pretty (go (i + 1) inner)

scopePrettyName :: Scope -> PrettyName
scopePrettyName (ManualScope namePretty _) = namePretty
