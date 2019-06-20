{-# LANGUAGE OverloadedStrings #-}
module Lib (myTerm) where

import Data.Text (Text)
import Syntax(Term(..), Var(..), abstract)

fun :: Text -> Term -> Term -> Term
fun name ty body = Lambda ty (abstract name body)

-- (+->) :: Term -> Term -> Term
-- t1 +-> t2 = Pi t1 (abstract "__unused__" t2)

type_ :: Int -> Term
type_ = Universe

v :: Text -> Term
v name = Var (Free name)

(@@) :: Term -> Term -> Term
t1 @@ t2 = App t1 t2

infixl 9 @@
-- infixr 8 +->

myTerm :: Term
myTerm = fun "y" (type_ 0) (fun "x" (type_ 0) (v "x") @@ v "y")

-- run :: Text -> (Text, Text)
-- run source = case parsed of
--   Left err -> ("parse error: " <> err, "")
--   Right t -> case typeCheck t of
--     Left err -> ("type error: " <> err, "")
--     Right ty -> ("Type: " <> prettyPrint (normalize ty), "Normalized: " <> prettyPrint (normalize t))
--   where parsed = parseTerm source

-- doFile :: String -> IO ()
-- doFile filename = do
--   source <- TIO.readFile filename
--   let (a, b) = run source
--   TIO.putStrLn a
--   TIO.putStrLn ""
--   TIO.putStrLn b
