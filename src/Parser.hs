{-# LANGUAGE EmptyDataDeriving #-}
module Parser (convertToDeBruijn, RawTerm(..), RawScope(..)) where

import Text.Megaparsec (Parsec, try, notFollowedBy, between, eof)
import Text.Megaparsec.Char (space1, string, letterChar, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, (<|>))
import qualified Syntax as S
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (void)

data RawTerm = Var String
             | Universe Int
             | Pi RawScope
             | Lambda RawScope
             | App RawTerm RawTerm

data RawScope = RawScope String RawTerm RawTerm

-- uninhabited type
data Void deriving (Eq, Ord)

-- we use String for error messages
type Parser = Parsec Void String

-- partially based on https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser ()
symbol x = L.symbol sc x >> pure ()

rword :: String -> Parser ()
rword w = lexeme (try (string w *> notFollowedBy alphaNumChar))

reservedWords :: [String]
reservedWords = ["fun", "forall", "Type"]

identifier :: Parser String
identifier = lexeme (try (p >>= check))
  where p = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                  else return x

progParser :: Parser RawTerm
progParser = between sc eof term

term :: Parser RawTerm
term = undefined

term' :: Parser RawTerm
term' = universe
    <|> var
    <|> piType
    <|> lambda
    <|> parens term

universe :: Parser RawTerm
universe = Universe <$> (rword "Type" *> L.decimal)

var :: Parser RawTerm
var = Var <$> identifier

piType :: Parser RawTerm
piType = do
  rword "forall"
  name <- identifier
  symbol ":"
  ty <- term
  symbol ","
  body <- term
  pure (Pi (RawScope name ty body))

lambda :: Parser RawTerm
lambda = do
  rword "fun"
  name <- identifier
  symbol ":"
  ty <- term
  symbol "=>"
  body <- term
  pure (Lambda (RawScope name ty body))

convertToDeBruijn :: RawTerm -> S.Term
convertToDeBruijn = go []
  where go :: [String] -> RawTerm -> S.Term
        go env (Var name) = S.Var (fromJust (elemIndex name env))
        go _ (Universe k) = S.Universe k
        go env (Pi rawScope) = S.Pi (goScope env rawScope)
        go env (Lambda rawScope) = S.Lambda (goScope env rawScope)
        go env (App t1 t2) = S.App (go env t1) (go env t2)

        goScope :: [String] -> RawScope -> S.Scope
        goScope env (RawScope name ty body) = S.Scope name (go env ty) (go (name:env) body)
