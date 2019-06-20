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
  , eq
  , refl
  , eqelim
  , ex
  , pack
  , fst_
  , snd_
  , or_
  , inl
  , inr
  , orelim
  , and_
  , pair
  , proj1
  , proj2
  , unit
  , tt
  , unitelim
  , void
  , voidelim
  )
where

import Deptt.Core.Syntax (Term(..), Var(..), Builtin(..), abstract)
import Data.Text (Text)

fun :: Text -> Term -> Term -> Term
fun name ty body = Lambda ty (abstract name body)

forall :: Text -> Term -> Term -> Term
forall name ty body = Pi ty (abstract name body)

(+->) :: Term -> Term -> Term
t1 +-> t2 = Pi t1 (abstract "__unused__" t2)

type_ :: Int -> Term
type_ = Universe

v :: Text -> Term
v name = Var (Free name)

(@@) :: Term -> Term -> Term
t1 @@ t2 = App t1 t2

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

or_ :: Term
or_ = Builtin Or

inl :: Term
inl = Builtin InL

inr :: Term
inr = Builtin InR

orelim :: Term
orelim = Builtin OrElim

and_ :: Term
and_ = Builtin And

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

infixl 9 @@
infixr 8 +->
