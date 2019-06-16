{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( run
    ) where

import Data.Text (Text)
import Parser (parseNoFail)
import PrettyPrint (prettyPrint)

-- run :: Text -> Text
-- run source = case parseTerm source of
--   Left err -> "error:\n" <> err
--   Right tm -> undefined
run :: Text
run = prettyPrint (parseNoFail "nat (nat -> nat)")
