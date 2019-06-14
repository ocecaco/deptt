module Parser (convertToDeBruijn, Term(..), Binder(..), parseTerm, runParse, parseNoFail) where

import Text.Megaparsec (Parsec, try, notFollowedBy, between, eof, parse)
import Text.Megaparsec.Char (space1, string, letterChar, alphaNumChar)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, (<|>))
import qualified Syntax as S
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Void (Void)

data Term = Var String
          | Universe Int
          | Pi Binder
          | Lambda Binder
          | Let Term Binder
          | App Term Term
          | Builtin S.Builtin

data Binder = Binder String Term Term

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
reservedWords = ["fun", "forall", "Type", "let", "in"]

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
        arrow t1 t2 = Pi (Binder "_" t1 t2)

term' :: Parser Term
term' = universe
    <|> var
    <|> piType
    <|> lambda
    <|> letdef
    <|> parens term

universe :: Parser Term
universe = Universe <$> (rword "Type" *> integer)

builtin :: String -> Maybe S.Builtin
builtin "nat" = Just S.Nat
builtin "zero" = Just S.Zero
builtin "succ" = Just S.Succ
builtin "natelim" = Just S.NatElim
builtin "eq" = Just S.Eq
builtin "refl" = Just S.Refl
builtin "eqelim" = Just S.EqElim
builtin _ = Nothing

-- TODO: Maybe allow shadowing of builtins
var :: Parser Term
var = do
  i <- identifier
  case builtin i of
    Just t -> return (Builtin t)
    Nothing -> return (Var i)

piType :: Parser Term
piType = do
  rword "forall"
  name <- identifier
  symbol ":"
  ty <- term
  symbol ","
  body <- term
  pure (Pi (Binder name ty body))

lambda :: Parser Term
lambda = do
  rword "fun"
  name <- identifier
  symbol ":"
  ty <- term
  symbol "=>"
  body <- term
  pure (Lambda (Binder name ty body))

letdef :: Parser Term
letdef = do
  rword "let"
  name <- identifier
  symbol ":"
  ty <- term
  symbol "="
  def <- term
  symbol "in"
  body <- term
  pure (Let def (Binder name ty body))

-- TODO: Handle scoping errors
convertToDeBruijn :: Term -> S.Term
convertToDeBruijn = go []
  where go :: [String] -> Term -> S.Term
        go env (Var name) = S.Var (fromJust (elemIndex name env))
        go _ (Universe k) = S.Universe k
        go env (Pi rawScope) = S.Pi (goScope env rawScope)
        go env (Lambda rawScope) = S.Lambda (goScope env rawScope)
        go env (Let def rawScope) = S.Let (go env def) (goScope env rawScope)
        go env (App t1 t2) = S.App (go env t1) (go env t2)
        go _ (Builtin b) = S.Builtin b

        goScope :: [String] -> Binder -> S.Binder
        goScope env (Binder name ty body) = S.Binder name (go env ty) (go (name:env) body)

parseTerm :: String -> Either String S.Term
parseTerm source = case parse progParser "<interactive>" source of
  Left errors -> Left $ "parse error:\n" ++ errorBundlePretty errors
  Right t -> Right (convertToDeBruijn t)

runParse :: String -> IO ()
runParse source = case parseTerm source of
  Left e -> putStrLn e
  Right t -> print t

parseNoFail :: String -> S.Term
parseNoFail str =
  case parseTerm str of
    Left _ -> error "parsing is assumed not to fail in parseNoFail"
    Right t -> t
