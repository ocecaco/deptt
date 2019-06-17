{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    , doFile
    ) where

import Data.Text (Text)
import Parser (parseTerm)
import PrettyPrint (prettyPrint)
import Normalize (normalize)
import TypeCheck (typeCheck)
import qualified Data.Text.IO as TIO

run :: Text -> (Text, Text)
run source = case parsed of
  Left err -> ("parse error: " <> err, "")
  Right t -> case typeCheck t of
    Left err -> ("type error: " <> err, "")
    Right ty -> ("Type: " <> prettyPrint (normalize ty), "Normalized: " <> prettyPrint (normalize t))
  where parsed = parseTerm source

doFile :: String -> IO ()
doFile filename = do
  source <- TIO.readFile filename
  let (a, b) = run source
  TIO.putStrLn a
  TIO.putStrLn ""
  TIO.putStrLn b
