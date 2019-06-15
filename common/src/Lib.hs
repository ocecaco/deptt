module Lib
    ( someFunc
    ) where

import Syntax (prettyPrint)
import TypeCheck (typeCheck, normalize)
import Parser (parseTerm)
import System.Environment (getArgs)
import System.IO (readFile)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  let filename = head args
  contents <- readFile filename
  let Right term = parseTerm contents
  let ty = prettyPrint <$> typeCheck term
  print ty
  putStrLn (prettyPrint (normalize term))
