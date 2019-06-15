{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom (mainWidget, el, text)
import Syntax (Term(..))
import qualified Data.Text as T

main = mainWidget $ el "div" $ text (T.pack (show (Var 0)))
