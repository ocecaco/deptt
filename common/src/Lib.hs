{-# LANGUAGE OverloadedStrings #-}
module Lib (myTerm, myTerm2) where

import Data.Text (Text)
import Syntax(Term(..), Var(..), abstract)
import SyntaxBuilder

myTerm :: Term
myTerm = plus two two
  where two = succ_ @@ (succ_ @@ zero)
        plus x y = natelim @@ fun "p" nat nat @@ x @@ fun "p" nat succ_ @@ y

myTerm2 :: Term
myTerm2 = refl @@ nat @@ zero

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
