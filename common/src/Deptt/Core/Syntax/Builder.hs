{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.Syntax.Builder
  ( fun
  , forall
  , (+->)
  , type_
  , v
  , (@@)
  , nat
  , zero
  , succ_
  , natelim
  , list
  , nil
  , cons
  , listelim
  , eq
  , refl
  , eqelim
  , ex
  , pack
  , fst_
  , snd_
  , sum_
  , inl
  , inr
  , sumelim
  , prod
  , pair
  , proj1
  , proj2
  , unit
  , tt
  , unitelim
  , void
  , voidelim
  , level
  , lzero
  , lsucc
  , lmax
  , universeTop
  )
where

import Deptt.Core.Syntax (Term(..), Var(..), Builtin(..), PrettyName(..), Name(..), abstract)
import Data.Text (Text)

fun :: Text -> Term -> Term -> Term
fun name ty body = Lambda ty (abstract name (PrettyName name) body)

forall :: Text -> Term -> Term -> Term
forall name ty body = Pi ty (abstract name (PrettyName name) body)

(+->) :: Term -> Term -> Term
t1 +-> t2 = Pi t1 (abstract "__unused__" (PrettyName "__unused__") t2)

type_ :: Term -> Term
type_ lvl = Builtin Universe @@ lvl

v :: Text -> Term
v name = Var (Free (Name name (PrettyName name)))

(@@) :: Term -> Term -> Term
t1 @@ t2 = t1 :@ t2

nat :: Term
nat = Builtin Nat

zero :: Term
zero = Builtin Zero

succ_ :: Term
succ_ = Builtin Succ

natelim :: Term
natelim = Builtin NatElim

eq :: Term
eq = Builtin Eq

refl :: Term
refl = Builtin Refl

eqelim :: Term
eqelim = Builtin EqElim

ex :: Term
ex = Builtin Ex

pack :: Term
pack = Builtin Pack

fst_ :: Term
fst_ = Builtin Fst

snd_ :: Term
snd_ = Builtin Snd

sum_ :: Term
sum_ = Builtin Sum

inl :: Term
inl = Builtin InL

inr :: Term
inr = Builtin InR

sumelim :: Term
sumelim = Builtin SumElim

prod :: Term
prod = Builtin Prod

pair :: Term
pair = Builtin Pair

proj1 :: Term
proj1 = Builtin Proj1

proj2 :: Term
proj2 = Builtin Proj2

unit :: Term
unit = Builtin Unit

tt :: Term
tt = Builtin Tt

unitelim :: Term
unitelim = Builtin UnitElim

void :: Term
void = Builtin Void

voidelim :: Term
voidelim = Builtin VoidElim

level :: Term
level = Builtin Level

lzero :: Term
lzero = Builtin LevelZero

lsucc :: Term
lsucc = Builtin LevelSucc

lmax :: Term
lmax = Builtin LevelMax

universeTop :: Term
universeTop = Builtin UniverseTop

list :: Term
list = Builtin List

nil :: Term
nil = Builtin Nil

cons :: Term
cons = Builtin Cons

listelim :: Term
listelim = Builtin ListElim

infixl 9 @@
infixr 8 +->
