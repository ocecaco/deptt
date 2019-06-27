{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck.Builtin (builtinType) where

import Deptt.Core.Syntax (Term, Builtin(..))
import Deptt.Core.Syntax.Builder

forlvl :: Term -> Term
forlvl = forall "lvl" level

lvl :: Term
lvl = v "lvl"

builtinType :: Builtin -> Maybe Term
builtinType Nat = Just $ type_ lzero
builtinType Zero = Just nat
builtinType Succ = Just $ nat +-> nat
builtinType NatElim = Just $ forlvl $ forall "P" (nat +-> type_ lvl) (v "P" @@ zero +-> forall "k" nat (v "P" @@ v "k" +-> v "P" @@ (succ_ @@ v "k")) +-> forall "n" nat (v "P" @@ v "n"))

builtinType Eq = Just $ forlvl $ forall "A" (type_ lvl) (v "A" +-> v "A" +-> type_ lvl)
builtinType Refl = Just $ forlvl $ forall "A" (type_ lvl) (forall "x" (v "A") (eq @@ lvl @@ v "A" @@ v "x" @@ v "x"))
builtinType EqElim = Just $ forlvl $ forall "A" (type_ lvl) (forall "x" (v "A") (forall "P" (v "A" +-> type_ lvl) (v "P" @@ v "x" +-> forall "y" (v "A") (eq @@ lvl @@ v "A" @@ v "x" @@ v "y" +-> v "P" @@ v "y"))))

-- TODO: more flexible levels
builtinType Ex = Just $ forlvl $ forall "A" (type_ lvl) ((v "A" +-> type_ lvl) +-> type_ lvl)
builtinType Pack = Just $ forlvl $ forall "A" (type_ lvl) (forall "P" (v "A" +-> type_ lvl) (forall "x" (v "A") (v "P" @@ v "x" +-> ex @@ lvl @@ v"A" @@ v "P")))
builtinType Fst = Just $ forlvl $ forall "A" (type_ lvl) (forall "P" (v "A" +-> type_ lvl) (ex @@ lvl @@ v "A" @@ v "P" +-> v "A"))
builtinType Snd = Just $ forlvl $ forall "A" (type_ lvl) (forall "P" (v "A" +-> type_ lvl) (forall "H" (ex @@ lvl @@ v "A" @@ v "P") (v "P" @@ (fst_ @@ lvl @@ v "A" @@ v "P" @@ v "H"))))

builtinType Or = Just $ forlvl $ type_ lvl +-> type_ lvl +-> type_ lvl
builtinType InL = Just $ forlvl $ forall "A" (type_ lvl) (forall "B" (type_ lvl) (v "A" +-> or_ @@ lvl @@ v "A" @@ v "B"))
builtinType InR = Just $ forlvl $ forall "A" (type_ lvl) (forall "B" (type_ lvl) (v "B" +-> or_ @@ lvl @@ v "A" @@ v "B"))
builtinType OrElim = Just $ forlvl $ forall "A" (type_ lvl) $ forall "B" (type_ lvl) $ forall "P" (or_ @@ lvl @@ v "A" @@ v "B" +-> type_ lvl) $ forall "a" (v "A") $ v "P" @@ (inl @@ lvl @@ v "A" @@ v "B" @@ v "a") +-> (forall "b" (v "B") (v "P" @@ (inr @@ lvl @@ v "A" @@ v "B" @@ v "b"))) +-> forall "s" (or_ @@ lvl @@ v "A" @@ v "B") (v "P" @@ v "s")

builtinType And = Just $ forlvl $ type_ lvl +-> type_ lvl +-> type_ lvl
builtinType Pair = Just $ forlvl $ forall "A" (type_ lvl) (forall "B" (type_ lvl) (v "A" +-> v "B" +-> and_ @@ lvl @@ v "A" @@ v "B"))
builtinType Proj1 = Just $ forlvl $ forall "A" (type_ lvl) (forall "B" (type_ lvl) (and_ @@ lvl @@ v "A" @@ v "B" +-> v "A"))
builtinType Proj2 = Just $ forlvl $ forall "A" (type_ lvl) (forall "B" (type_ lvl) (and_ @@ lvl @@ v "A" @@ v "B" +-> v "B"))

builtinType Unit = Just $ forlvl $ type_ lvl
builtinType Tt = Just $ forlvl $ unit @@ lvl
builtinType UnitElim = Just $ forlvl $ forall "P" (unit @@ lvl +-> type_ lvl) (v "P" @@ (tt @@ lvl) +-> forall "u" (unit @@ lvl) (v "P" @@ v "u"))

builtinType Void = Just $ forlvl $ type_ lvl
builtinType VoidElim = Just $ forlvl $ forall "P" (void @@ lvl +-> type_ lvl) (forall "e" (void @@ lvl) (v "P" @@ v "e"))

builtinType Level = Just $ type_ lzero
builtinType LevelZero = Just level
builtinType LevelSucc = Just $ level +-> level
builtinType LevelMax = Just $ level +-> level +-> level

builtinType Universe = Just $ forall "lvl" level $ type_ (lsucc @@ v "lvl")

builtinType UniverseTop = Nothing
