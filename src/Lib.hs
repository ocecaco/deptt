module Lib
    ( someFunc
    ) where

import Syntax (Term, prettyPrint)
import TypeCheck (typeCheck, normalize)
import qualified Parser as P

someFunc :: IO ()
someFunc = do
  let numToNum = P.Pi (P.RawScope "_" (P.Var "N") (P.Var "N"))
  let introN body = P.Lambda (P.RawScope "N" (P.Universe 0) body)
  let introsucc body = P.Lambda (P.RawScope "succ" numToNum body)
  let introzero body = P.Lambda (P.RawScope "zero" (P.Var "N") body)
  let addtwo = P.Lambda (P.RawScope "num" (P.Var "N") (P.App (P.Var "succ") (P.App (P.Var "succ") (P.Var "num"))))
  let introaddtwo body = P.App (P.Lambda (P.RawScope "addtwo" numToNum body)) addtwo
  let mybody = P.App (P.Var "addtwo") (P.App (P.Var "addtwo") (P.Var "zero"))

  let myterm = P.convertToDeBruijn (introN (introsucc (introzero (introaddtwo mybody))))

  print (typeCheck myterm)
  putStrLn (prettyPrint (normalize myterm))
