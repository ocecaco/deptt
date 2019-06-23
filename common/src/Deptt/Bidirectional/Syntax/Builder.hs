{-# LANGUAGE OverloadedStrings #-}
module Deptt.Bidirectional.Syntax.Builder
  ( v
  , type_
  , (@@)
  , (+->)
  , fun
  , let_
  , forall
  , (&:)
  , (=%=)
  , refl
  , ToTerm
  )
where

import Deptt.Bidirectional.Syntax
import Data.Text (Text)

newtype Wrap = Wrap { unwrap :: TermC }

-- This class makes sure we can use a TermI where a TermC is
-- expected. It essentially coerces a TermI into a TermC when
-- necessary.
class ToTerm t where
  convert :: t -> Wrap

instance ToTerm TermC where
  convert = Wrap

instance ToTerm TermI where
  convert = Wrap . Infer

v :: Text -> TermI
v name = Var (Free name)

type_ :: Int -> TermI
type_ = Universe

(@@) :: ToTerm t => TermI -> t -> TermI
t1 @@ t2 = App t1 (unwrap (convert t2))

(+->) :: TermI -> TermI -> TermI
ty1 +-> ty2 = Pi ty1 (abstractI "__unused__" ty2)

fun :: ToTerm t => Text -> t -> TermC
fun name body = Lambda (abstractC name (unwrap (convert body)))

let_ :: ToTerm t => Text -> TermI -> t -> TermC
let_ name def body = Let def (abstractC name (unwrap (convert body)))

forall :: Text -> TermI -> TermI -> TermI
forall name ty body = Pi ty (abstractI name body)

(&:) :: ToTerm t => t -> TermI -> TermI
tm &: ty = Annotate (unwrap (convert tm)) ty

(=%=) :: TermI -> TermI -> TermI
t1 =%= t2 = Eq t1 t2

refl :: TermC
refl = Refl

infixl 8 @@
infix 7 =%=
infixr 6 +->
infix 5 &:
