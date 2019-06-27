{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck.Builtin (builtinType) where

import Deptt.Core.Syntax (Term, Builtin(..))
import Deptt.Core.Syntax.Builder
import Data.Text (Text)

forlvl :: Text -> Term -> Term
forlvl name = forall name level

lvl :: Term
lvl = v "lvl"

lvl1 :: Term
lvl1 = v "lvl1"

lvl2 :: Term
lvl2 = v "lvl2"

lvl3 :: Term
lvl3 = v "lvl3"

builtinType :: Builtin -> Maybe Term
builtinType Nat = Just $ type_ lzero
builtinType Zero = Just nat
builtinType Succ = Just $ nat +-> nat
builtinType NatElim = Just $ forlvl "lvl" $ forall "P" (nat +-> type_ lvl) (v "P" @@ zero +-> forall "k" nat (v "P" @@ v "k" +-> v "P" @@ (succ_ @@ v "k")) +-> forall "n" nat (v "P" @@ v "n"))

builtinType Eq = Just $ forlvl "lvl" $ forall "A" (type_ lvl) (v "A" +-> v "A" +-> type_ lvl)
builtinType Refl = Just $ forlvl "lvl" $ forall "A" (type_ lvl) (forall "x" (v "A") (eq @@ lvl @@ v "A" @@ v "x" @@ v "x"))
builtinType EqElim = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "x" (v "A") (forall "P" (v "A" +-> type_ lvl2) (v "P" @@ v "x" +-> forall "y" (v "A") (eq @@ lvl1 @@ v "A" @@ v "x" @@ v "y" +-> v "P" @@ v "y"))))

builtinType Ex = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) ((v "A" +-> type_ lvl2) +-> type_ (lmax @@ lvl1 @@ lvl2))
builtinType Pack = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "P" (v "A" +-> type_ lvl2) (forall "x" (v "A") (v "P" @@ v "x" +-> ex @@ lvl1 @@ lvl2 @@ v"A" @@ v "P")))
builtinType Fst = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "P" (v "A" +-> type_ lvl2) (ex @@ lvl1 @@ lvl2 @@ v "A" @@ v "P" +-> v "A"))
builtinType Snd = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "P" (v "A" +-> type_ lvl2) (forall "H" (ex @@ lvl1 @@ lvl2 @@ v "A" @@ v "P") (v "P" @@ (fst_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "P" @@ v "H"))))

builtinType Or = Just $ forlvl "lvl1" $ forlvl "lvl2" $ type_ lvl1 +-> type_ lvl2 +-> type_ (lmax @@ lvl1 @@ lvl2)
builtinType InL = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "B" (type_ lvl2) (v "A" +-> or_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B"))
builtinType InR = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "B" (type_ lvl2) (v "B" +-> or_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B"))
builtinType OrElim = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forlvl "lvl3" $ forall "A" (type_ lvl1) $ forall "B" (type_ lvl2) $ forall "P" (or_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B" +-> type_ lvl3) $ forall "a" (v "A") $ v "P" @@ (inl @@ lvl1 @@ lvl2 @@ v "A" @@ v "B" @@ v "a") +-> (forall "b" (v "B") (v "P" @@ (inr @@ lvl1 @@ lvl2 @@ v "A" @@ v "B" @@ v "b"))) +-> forall "s" (or_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B") (v "P" @@ v "s")

builtinType And = Just $ forlvl "lvl1" $ forlvl "lvl2" $ type_ lvl1 +-> type_ lvl2 +-> type_ (lmax @@ lvl1 @@ lvl2)
builtinType Pair = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "B" (type_ lvl2) (v "A" +-> v "B" +-> and_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B"))
builtinType Proj1 = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "B" (type_ lvl2) (and_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B" +-> v "A"))
builtinType Proj2 = Just $ forlvl "lvl1" $ forlvl "lvl2" $ forall "A" (type_ lvl1) (forall "B" (type_ lvl2) (and_ @@ lvl1 @@ lvl2 @@ v "A" @@ v "B" +-> v "B"))

builtinType Unit = Just $ type_ lzero
builtinType Tt = Just unit
builtinType UnitElim = Just $ forlvl "lvl" $ forall "P" (unit +-> type_ lvl) (v "P" @@ tt +-> forall "u" unit (v "P" @@ v "u"))

builtinType Void = Just $ type_ lzero
builtinType VoidElim = Just $ forlvl "lvl" $ forall "P" (void +-> type_ lvl) (forall "e" void (v "P" @@ v "e"))

builtinType Level = Just $ type_ lzero
builtinType LevelZero = Just level
builtinType LevelSucc = Just $ level +-> level
builtinType LevelMax = Just $ level +-> level +-> level

builtinType Universe = Just $ forall "lvl" level $ type_ (lsucc @@ v "lvl")

builtinType UniverseTop = Nothing
