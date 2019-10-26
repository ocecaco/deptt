{-# LANGUAGE OverloadedStrings #-}
module Deptt (run) where

import Deptt.Core.Normalize
import Deptt.Core.TypeCheck
import Deptt.Core.PrettyPrint
import Deptt.Core.Parser
import Deptt.Core.Lexer
import qualified Deptt.Core.TypeCheck.Monad as M
import Data.Text (Text)
import qualified Data.Text as T

run :: Text -> (Text, Text)
run source = case parsed of
  Right t -> case typeCheck t of
    Left err -> ("type error: " <> err, "")
    Right ty -> ("Type: " <> prettyPrint (norm ty), "Normalized: " <> prettyPrint (norm t))
  Left _ -> error "parse error"
  where parsed = convertToDeBruijn (parse (scanTokens (T.unpack source)))
        norm tm = case M.run (normalize tm) of
          Left _ -> error "error while normalizing"
          Right n -> n
