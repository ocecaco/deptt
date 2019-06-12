module Lib
    ( someFunc
    ) where

import Syntax (prettyPrint)
import TypeCheck (typeCheck, normalize)
import qualified Parser as P

lambda :: String -> P.Term -> P.Term -> P.Term
lambda x t b = P.Lambda (P.Binder (Just x) t b)

pi :: String -> P.Term -> P.Term -> P.Term
pi x t b = P.Pi (P.Binder (Just x) t b)

arrow :: P.Term -> P.Term -> P.Term
arrow t1 t2 = P.Pi (P.Binder Nothing t1 t2)

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
  -- let myterm = P.convertToDeBruijn (lambda "A" (P.Universe 0) (lambda "B" (P.Universe 0) (lambda "f" (arrow (P.Var "A") (P.Var "B")) (lambda "x" (P.Var "A") (P.App (P.Var "f") (P.Var "x"))))))

  print (prettyPrint <$> typeCheck myterm)
  putStrLn (prettyPrint (normalize myterm))
