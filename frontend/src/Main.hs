{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import qualified Deptt as L
import Data.Monoid

main :: IO ()
main = mainWidget $ elClass "section" "section" $ elClass "div" "container" $ do
  elClass "h1" "title" (text "Type checker for dependent type theory")
  input <- el "div" $ textArea $ def
    & textAreaConfig_initialValue .~ "let lone : level = lsucc lzero\n\nand type0 : type lone = type lzero\n\nand type1 : type (lsucc lone) = type lone\n\nand binaryrelation (A : type0) : type1 = A -> A -> type0\n\nand reflexive (A : type0) (rel : binaryrelation A) : type0 = forall x : A, rel x x\n\nand symmetric (A : type0) (rel : binaryrelation A) : type0 = forall x y : A, rel x y -> rel y x\n\nand transitive (A : type0) (rel : binaryrelation A) : type0 = forall x y z : A, rel x y -> rel y z -> rel x z\n\nand equivrelation (A : type0) (rel : binaryrelation A) : type0 = prod lzero lzero (reflexive A rel) (prod lzero lzero (symmetric A rel) (transitive A rel))\n\nand setoid : type1 = ex lone lone type0 (fun A : type0 => ex lone lzero (binaryrelation A) (fun rel : binaryrelation A => equivrelation A rel))\n\nand relationby (A B : type0) (f : A -> B) : binaryrelation A = fun (x y : A) => eq lzero B (f x) (f y)\n\nin\n\nlet plus (n m : nat) : nat = natelim lzero (fun p : nat => nat) m (fun p : nat => succ) n\n\nin\n\nnatelim lzero\n\n(fun q : nat => eq lzero nat (plus q zero) q)\n\n(refl lzero nat zero)\n\n(\nfun (k : nat) (IH : eq lzero nat (plus k zero) k) =>\neqelim lzero lzero nat (plus k zero) (fun t : nat => eq lzero nat (plus (succ k) zero) (succ t)) (refl lzero nat (plus (succ k) zero)) k IH\n)\n\n: forall n : nat, eq lzero nat (plus n zero) n\n"
    & textAreaConfig_attributes .~ constDyn ("rows" =: "50" <> "cols" =: "140")
  btn <- el "div" $ button "Go"
  let terms = tag (fmap L.run . current $ _textArea_value input) btn
  _ <- el "div" $ el "pre" $ el "code" $ textNode $ def
    & textNodeConfig_setContents .~ fmap fst terms
  _ <- el "div" $ el "pre" $ el "code" $ textNode $ def
    & textNodeConfig_setContents .~ fmap snd terms
  return ()
