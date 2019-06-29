{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.Parser (convertToDeBruijn, Term(..), Scope(..), parseTerm) where

import Text.Megaparsec (Parsec, try, notFollowedBy, between, eof, parse, parseErrorPretty, sepBy1, some)
import Text.Megaparsec.Char (space1, string, letterChar, alphaNumChar, char)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative (many, (<|>))
import qualified Deptt.Core.Syntax as S
import Data.List (elemIndex, concat, concat, concat, concat, concat, concat, concat, concat)
import Data.Void (Void)
import qualified Data.Text as T
import Data.Text (Text)

data Term = Var Text
          | Pi Term Scope
          | Lambda Term Scope
          | Let Term Term Scope
          | App Term Term
          | Builtin S.Builtin

data Scope = Scope Text Term

type Parser = Parsec Void Text

-- partially based on https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where lineComment = L.skipLineComment "--"
        blockComment = L.skipBlockComment "{-" "-}"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol x = L.symbol sc x >> pure ()

rword :: Text -> Parser ()
rword w = lexeme (try (string w *> notFollowedBy alphaNumChar))

reservedWords :: [Text]
reservedWords = ["fun", "forall", "let", "in"]

identifier :: Parser Text
identifier = lexeme (try (name >>= check))
  where name = T.cons <$> letterChar <*> (T.pack <$> many (alphaNumChar <|> char '\''))
        check x = if x `elem` reservedWords
                  then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
                  else return x

binders :: Parser [(Text, Term)]
binders = manybinders <|> singlebinder

manybinders :: Parser [(Text, Term)]
manybinders = do
  groups <- some (parens singlebinder)
  return (concat groups)

singlebinder :: Parser [(Text, Term)]
singlebinder = do
  idents <- identifier `sepBy1` sc
  symbol ":"
  ty <- term
  return [ (i, ty) | i <- idents ]

progParser :: Parser Term
progParser = between sc eof term

term :: Parser Term
term = makeExprParser term'
  [ [ InfixL (App <$ sc) ]
  , [ InfixR (arrow <$ symbol "->") ] ]
  where arrow :: Term -> Term -> Term
        arrow t1 t2 = Pi t1 (Scope "_" t2)

term' :: Parser Term
term' = var
    <|> piType
    <|> lambda
    <|> letdef
    <|> parens term

builtin :: Text -> Maybe S.Builtin
builtin "nat" = Just S.Nat
builtin "zero" = Just S.Zero
builtin "succ" = Just S.Succ
builtin "natelim" = Just S.NatElim

builtin "list" = Just S.List
builtin "nil" = Just S.Nil
builtin "cons" = Just S.Cons
builtin "listelim" = Just S.ListElim

builtin "eq" = Just S.Eq
builtin "refl" = Just S.Refl
builtin "eqelim" = Just S.EqElim

builtin "ex" = Just S.Ex
builtin "pack" = Just S.Pack
builtin "fst" = Just S.Fst
builtin "snd" = Just S.Snd

builtin "or" = Just S.Or
builtin "inl" = Just S.InL
builtin "inr" = Just S.InR
builtin "orelim" = Just S.OrElim

builtin "and" = Just S.And
builtin "pair" = Just S.Pair
builtin "proj1" = Just S.Proj1
builtin "proj2" = Just S.Proj2

builtin "unit" = Just S.Unit
builtin "tt" = Just S.Tt
builtin "unitelim" = Just S.UnitElim

builtin "void" = Just S.Void
builtin "voidelim" = Just S.VoidElim

builtin "level" = Just S.Level
builtin "lzero" = Just S.LevelZero
builtin "lsucc" = Just S.LevelSucc
builtin "lmax" = Just S.LevelMax

builtin "type" = Just S.Universe
builtin "typeomega" = Just S.UniverseTop
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
  bs <- binders
  symbol ","
  body <- term
  pure (foldr (\(name, ty) tm -> Pi ty (Scope name tm)) body bs)

lambda :: Parser Term
lambda = do
  rword "fun"
  bs <- binders
  symbol "=>"
  body <- term
  pure (foldr (\(name, ty) tm -> Lambda ty (Scope name tm)) body bs)

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
  pure (Let def ty (Scope name body))

data ScopeError = OutOfScope Text

convertToDeBruijn :: Term -> Either ScopeError S.Term
convertToDeBruijn = go []
  where go :: [Text] -> Term -> Either ScopeError S.Term
        go env (Var name) =
          case elemIndex name env of
            Nothing -> Left $ OutOfScope name
            Just i -> Right $ S.Var (S.Bound i)

        go env (Pi ty rawScope) = S.Pi <$> go env ty <*> goScope env rawScope
        go env (Lambda ty rawScope) = S.Lambda <$> go env ty <*> goScope env rawScope
        go env (Let def ty rawScope) = S.Let <$> go env def <*> go env ty <*> goScope env rawScope
        go env (App t1 t2) = (S.:@) <$> go env t1 <*> go env t2
        go _ (Builtin b) = pure (S.Builtin b)

        goScope :: [Text] -> Scope -> Either ScopeError S.Scope
        goScope env (Scope name body) = S.ManualScope <$> go (name:env) body

parseTerm :: Text -> Either Text S.Term
parseTerm source = case parse progParser "<interactive>" source of
  Left e -> Left (T.pack (parseErrorPretty e))
  Right t -> case convertToDeBruijn t of
    Left (OutOfScope name) -> Left $ "variable out of scope: " <> name
    Right t2 -> Right t2

parseNoFail :: Text -> S.Term
parseNoFail str =
  case parseTerm str of
    Left _ -> error "parsing is assumed not to fail in parseNoFail"
    Right t -> t