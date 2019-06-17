module Normalize (normalize) where

import Syntax (Term(..), Builtin(..), Binder(..), scopeApply)

natElim :: Term -> Term -> Term -> Term -> Term
natElim p b i n = App (App (App (App (Builtin NatElim) p) b) i) n

-- TODO: computation rule for eqelim
normalizeBuiltin :: Term -> Term -> Maybe Term
-- natelim: base case
normalizeBuiltin (App (App (App (Builtin NatElim) _) nbase) _) (Builtin Zero) = Just nbase
-- natelim: inductive case
normalizeBuiltin (App (App (App (Builtin NatElim) nprop) nbase) nind) (App (Builtin Succ) k) = Just (normalize (App (App nind k) (natElim nprop nbase nind k)))
normalizeBuiltin (App (App (Builtin Fst) _) _) (App (App (App (App (Builtin ExIntro) _) _) x) _) = Just x
normalizeBuiltin (App (App (Builtin Snd) _) _) (App (App (App (App (Builtin ExIntro) _) _) _) y) = Just y
normalizeBuiltin _ _ = Nothing

normalize :: Term -> Term
normalize tm@(Var _) = tm
normalize tm@(Universe _) = tm
normalize (Let def scope) = normalize (scopeApply scope (normalize def))
normalize (App e1old e2old) = case normalizeBuiltin e1norm e2norm of
  Just result -> result
  Nothing -> case e1norm of
    Lambda scope -> normalize (scopeApply scope e2norm)
    _ -> App e1norm e2norm
  where e2norm = normalize e2old
        e1norm = normalize e1old
normalize tm@(Builtin _) = tm

normalize (Pi scope) = Pi (normalizeScope scope)
normalize (Lambda scope) = Lambda (normalizeScope scope)

normalizeScope :: Binder -> Binder
normalizeScope (Binder name ty body) = Binder name (normalize ty) (normalize body)
