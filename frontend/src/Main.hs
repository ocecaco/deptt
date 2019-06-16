{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Reflex.Dom
import Data.Text (Text)
import qualified Lib as L

myButton :: DomBuilder t m => Text -> m (Event t ())
myButton t = do
  (e, _) <- elClass' "button" "button" (text t)
  return $ domEvent Click e

-- main :: IO ()
-- main = mainWidget $ elClass "section" "section" $ elClass "div" "container" $ do
--   elClass "h1" "title" (text "Type checker for dependent type theory")
--   input <- textArea def
--   btn <- myButton "Click me"
--   _ <- textNode $ def
--     & textNodeConfig_setContents .~ tag (fmap T.reverse . current $ _textArea_value input) btn
-- return ()
main :: IO ()
main = mainWidget $ el "div" $ text L.run
