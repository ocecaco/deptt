{-# LANGUAGE EmptyDataDeriving #-}
module Parser (convertToDeBruijn, Term(..), Binder(..), parseTerm, runParse) where

import Text.Megaparsec (Parsec, try, notFollowedBy, between, eof, parse)
import Text.Megaparsec.Char (space1, string, letterChar, alphaNumChar, char)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, (<|>))
import qualified Syntax as S
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Monad (void)
import Data.Void (Void)

data Term = Var String
          | Universe Int
          | Pi Binder
          | Lambda Binder
          | App Term Term

data Binder = Binder (Maybe String) Term Term

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

integer :: Parser Int
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = lexeme (try (string w *> notFollowedBy alphaNumChar))

reservedWords :: [String]
reservedWords = ["fun", "forall", "Type"]

maybeident :: Parser (Maybe String)
maybeident = dummy
         <|> (Just <$> identifier)
  where dummy = char '_' *> pure Nothing

identifier :: Parser String
identifier = lexeme (try (name >>= check))
  where name = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                  else return x

progParser :: Parser Term
progParser = between sc eof term

term :: Parser Term
term = makeExprParser term'
  [ [ InfixL (App <$ symbol "") ]
  , [ InfixR (arrow <$ symbol "->") ] ]
  where arrow :: Term -> Term -> Term
        arrow t1 t2 = Pi (Binder Nothing t1 t2)

term' :: Parser Term
term' = universe
    <|> var
    <|> piType
    <|> lambda
    <|> parens term

universe :: Parser Term
universe = Universe <$> (rword "Type" *> integer)

var :: Parser Term
var = Var <$> identifier

piType :: Parser Term
piType = do
  rword "forall"
  name <- maybeident
  symbol ":"
  ty <- term
  symbol ","
  body <- term
  pure (Pi (Binder name ty body))

lambda :: Parser Term
lambda = do
  rword "fun"
  name <- maybeident
  symbol ":"
  ty <- term
  symbol "=>"
  body <- term
  pure (Lambda (Binder name ty body))

-- TODO: Handle scoping errors
convertToDeBruijn :: Term -> S.Term
convertToDeBruijn = go []
  where go :: [Maybe String] -> Term -> S.Term
        go env (Var name) = S.Var (fromJust (elemIndex (Just name) env))
        go _ (Universe k) = S.Universe k
        go env (Pi rawScope) = S.Pi (goScope env rawScope)
        go env (Lambda rawScope) = S.Lambda (goScope env rawScope)
        go env (App t1 t2) = S.App (go env t1) (go env t2)

        goScope :: [Maybe String] -> Binder -> S.Binder
        goScope env (Binder name ty body) = S.Binder name (go env ty) (go (name:env) body)

parseTerm :: String -> Either String S.Term
parseTerm source = case parse progParser "<interactive>" source of
  Left errors -> Left $ "parse error:\n" ++ errorBundlePretty errors
  Right t -> Right (convertToDeBruijn t)

runParse :: String -> IO ()
runParse source = case parseTerm source of
  Left e -> putStrLn e
  Right t -> print t
