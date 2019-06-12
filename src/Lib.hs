module Lib
    ( someFunc
    ) where

import Syntax (prettyPrint)
import TypeCheck (typeCheck, normalize)
import qualified Parser as P

someFunc :: IO ()
someFunc = do
  let numToNum = P.Pi (P.Binder Nothing (P.Var "N") (P.Var "N"))
  let introN body = P.Lambda (P.Binder (Just "N") (P.Universe 0) body)
  let introsucc body = P.Lambda (P.Binder (Just "succ") numToNum body)
  let introzero body = P.Lambda (P.Binder (Just "zero") (P.Var "N") body)
  let addtwo = P.Lambda (P.Binder (Just "num") (P.Var "N") (P.App (P.Var "succ") (P.App (P.Var "succ") (P.Var "num"))))
  let introaddtwo body = P.App (P.Lambda (P.Binder (Just "addtwo") numToNum body)) addtwo
  let mybody = P.App (P.Var "addtwo") (P.App (P.Var "addtwo") (P.Var "zero"))

  let myterm = P.convertToDeBruijn (introN (introsucc (introzero (introaddtwo mybody))))

  print (typeCheck myterm)
  putStrLn (prettyPrint (normalize myterm))
