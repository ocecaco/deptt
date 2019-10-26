{
{-# OPTIONS_GHC -w #-}
module Deptt.Core.Parser
  ( parse
  , convertToDeBruijn
  , Term(..)
  )
where

import Deptt.Core.Lexer (Token(..))
import qualified Deptt.Core.Syntax as S
import Control.Applicative (Alternative(..))
import Data.List (elemIndex)
import qualified Data.Text as T
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  fun           { TokFun }
  forall        { TokForall }
  let           { TokLet }
  in            { TokIn }
  and           { TokAnd }
  '('           { TokLParen }
  ')'           { TokRParen }
  '->'          { TokArrow }
  '=>'          { TokFatArrow }
  ':'           { TokColon }
  '_'           { TokUnderscore }
  '='           { TokEquals }
  ','           { TokComma }
  ident         { TokIdent $$ }

%%

Term : fun ident ':' Term '=>' Term          { Lambda $2 $4 $6 }
     | forall ident ':' Term ',' Term        { Pi $2 $4 $6 }
     | let ident ':' Term '=' Term in Term   { Let $2 $4 $6 $8 }
     | Term1                                 { $1 }

Term1 : Term2 '->' Term    { Pi "__unused" $1 $3 }
      | Term2              { $1 }

Term2 : Term2 Term3        { App $1 $2 }
      | Term3              { $1 }

Term3 : ident              { Var $1 }
      | '(' Term ')'       { $2 }

{
data Term = Var String
          | Pi String Term Term
          | Lambda String Term Term
          | Let String Term Term Term
          | App Term Term
          deriving (Show)

matchBuiltin :: String -> Maybe S.Builtin
matchBuiltin "nat" = Just S.Nat
matchBuiltin "zero" = Just S.Zero
matchBuiltin "succ" = Just S.Succ
matchBuiltin "natelim" = Just S.NatElim

matchBuiltin "list" = Just S.List
matchBuiltin "nil" = Just S.Nil
matchBuiltin "cons" = Just S.Cons
matchBuiltin "listelim" = Just S.ListElim

matchBuiltin "eq" = Just S.Eq
matchBuiltin "refl" = Just S.Refl
matchBuiltin "eqelim" = Just S.EqElim

matchBuiltin "ex" = Just S.Ex
matchBuiltin "pack" = Just S.Pack
matchBuiltin "fst" = Just S.Fst
matchBuiltin "snd" = Just S.Snd

matchBuiltin "sum" = Just S.Sum
matchBuiltin "inl" = Just S.InL
matchBuiltin "inr" = Just S.InR
matchBuiltin "sumelim" = Just S.SumElim

matchBuiltin "prod" = Just S.Prod
matchBuiltin "pair" = Just S.Pair
matchBuiltin "proj1" = Just S.Proj1
matchBuiltin "proj2" = Just S.Proj2

matchBuiltin "unit" = Just S.Unit
matchBuiltin "tt" = Just S.Tt
matchBuiltin "unitelim" = Just S.UnitElim

matchBuiltin "void" = Just S.Void
matchBuiltin "voidelim" = Just S.VoidElim

matchBuiltin "level" = Just S.Level
matchBuiltin "lzero" = Just S.LevelZero
matchBuiltin "lsucc" = Just S.LevelSucc
matchBuiltin "lmax" = Just S.LevelMax

matchBuiltin "type" = Just S.Universe
matchBuiltin "typeomega" = Just S.UniverseTop
matchBuiltin _ = Nothing

data ScopeError = OutOfScope String

convertToDeBruijn :: Term -> Either ScopeError S.Term
convertToDeBruijn = go []
  where go :: [String] -> Term -> Either ScopeError S.Term
        go env (Var name) =
          case maybeResolved of
            Nothing -> Left $ OutOfScope name
            Just i -> Right i
          where maybeResolved = ((S.Var . S.Bound) <$> elemIndex name env) <|> (S.Builtin <$> matchBuiltin name)

        go env (Pi name ty body) = S.Pi <$> go env ty <*> goScope env name body
        go env (Lambda name ty body) = S.Lambda <$> go env ty <*> goScope env name body
        go env (Let name ty def body) = S.Let <$> go env def <*> go env ty <*> goScope env name body
        go env (App t1 t2) = (S.:@) <$> go env t1 <*> go env t2

        goScope :: [String] -> String -> Term -> Either ScopeError S.Scope
        goScope env name body = S.ManualScope (S.PrettyName (T.pack name)) <$> go (name:env) body

-- TODO: Better error handling
parseError :: [Token] -> a
parseError _ = error "parse error"
}
