{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    ) where

import Data.Text (Text)
import Parser (parseTerm)
import PrettyPrint (prettyPrint)
import Normalize (normalize)
import TypeCheck (typeCheck)

run :: Text -> (Text, Text)
run source = case parsed of
  Left err -> ("parse error: " <> err, "")
  Right t -> case typeCheck t of
    Left err -> ("type error: " <> err, "")
    Right ty -> ("Type: " <> prettyPrint (normalize ty), "Normalized: " <> prettyPrint (normalize t))
  where parsed = parseTerm source
