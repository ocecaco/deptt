module Lib
    ( someFunc
    ) where

import Syntax (Term(..), Scope(..), prettyPrint)

someFunc :: IO ()
someFunc = do
  let term1 = Lambda (Scope "x" (Universe 0) (Var 0))
  putStrLn (prettyPrint term1)
  let term2 = Pi (Scope "x" (Universe 2) (Var 0))
  putStrLn (prettyPrint term2)
  let term3 = App term1 term2
  putStrLn (prettyPrint term3)
