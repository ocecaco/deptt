module Deptt.Core.Syntax
  ( Var(..)
  , Term(..)
  , Scope(..)
  , Builtin(..)
  , Name(..)
  , PrettyName(..)
  , abstract
  , instantiate
  , scopePrettyName
  , isUnusedScope
  )
where

import Data.Text (Text)

newtype PrettyName = PrettyName { unwrapPrettyName :: Text }
                   deriving (Ord, Show)

-- don't take into account the pretty-printing names when comparing
-- terms for equality
instance Eq PrettyName where
  _ == _ = True

data Name = Name { internalName :: Text, prettyName :: PrettyName }
          deriving (Eq, Ord, Show)

data Var = Bound Int -- de Bruijn index
         | Free Name -- free variable by name
         deriving (Eq, Ord, Show)

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Var
          | Pi Term Scope
          | Lambda Term Scope
          | Let Term Term Scope
          | Term :@ Term
          | Annotate Term Term

          | Builtin Builtin
          deriving (Eq, Ord, Show)

infixl 8 :@

data Scope = ManualScope PrettyName Term
              deriving (Eq, Ord, Show)

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

             | Sum
             | InL
             | InR
             | SumElim

             | Prod
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
             deriving (Eq, Ord, Enum, Bounded, Show)

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
        go i (Annotate term ty) = Annotate (go i term) (go i ty)

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
        go i (Annotate term ty) = Annotate (go i term) (go i ty)

        goScope :: Int -> Scope -> Scope
        goScope i (ManualScope pretty inner) = ManualScope pretty (go (i + 1) inner)

scopePrettyName :: Scope -> PrettyName
scopePrettyName (ManualScope namePretty _) = namePretty

occursVar :: Int -> Term -> Bool
occursVar k (Var (Bound i)) = k == i
occursVar k (Pi ty scope) = occursVar k ty || occursVarScope k scope
occursVar k (Lambda ty scope) = occursVar k ty || occursVarScope k scope
occursVar k (Let ty def scope) = occursVar k ty || occursVar k def || occursVarScope k scope
occursVar k (t1 :@ t2) = occursVar k t1 || occursVar k t2
occursVar k (Annotate term ty) = occursVar k term || occursVar k ty
occursVar _ (Builtin _) = False
occursVar _ (Var (Free _)) = False

occursVarScope :: Int -> Scope -> Bool
occursVarScope i (ManualScope _ body) = occursVar (i + 1) body

isUnusedScope :: Scope -> Bool
isUnusedScope (ManualScope _ body) = not (occursVar 0 body)
