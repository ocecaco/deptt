{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import qualified Deptt as L

main :: IO ()
main = mainWidget $ elClass "section" "section" $ elClass "div" "container" $ do
  elClass "h1" "title" (text "Type checker for dependent type theory")
  input <- el "div" $ textArea def
  btn <- el "div" $ button "Go"
  let terms = tag (fmap L.run . current $ _textArea_value input) btn
  _ <- el "div" $ el "pre" $ el "code" $ textNode $ def
    & textNodeConfig_setContents .~ fmap fst terms
  _ <- el "div" $ el "pre" $ el "code" $ textNode $ def
    & textNodeConfig_setContents .~ fmap snd terms
  return ()
