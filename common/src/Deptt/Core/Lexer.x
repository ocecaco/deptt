{
{-# OPTIONS_GHC -w #-}

module Deptt.Core.Lexer
  ( Token(..)
  , scanTokens
  )
where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol = [\n]

tokens :-
  -- Skip whitespace
  $eol                        ;
  $white+                     ;

  -- Comments
  "--".*                      ;
  -- TODO: Multiline comments

  fun                             { \s -> TokFun }
  forall                          { \s -> TokForall }
  let                             { \s -> TokLet }
  in                              { \s -> TokIn }
  and                             { \s -> TokAnd }
  \(                              { \s -> TokLParen }
  \)                              { \s -> TokRParen }
  "->"                            { \s -> TokArrow }
  "=>"                            { \s -> TokFatArrow }
  \:                              { \s -> TokColon }
  \_                              { \s -> TokUnderscore }
  \=                              { \s -> TokEquals }
  \,                              { \s -> TokComma }
  $alpha [$alpha $digit \_ \']*   { \s -> TokIdent s}

{
data Token =
    TokFun
  | TokForall
  | TokLet
  | TokIn
  | TokAnd
  | TokLParen
  | TokRParen
  | TokArrow
  | TokFatArrow
  | TokColon
  | TokUnderscore
  | TokEquals
  | TokComma
  | TokIdent String
  deriving (Show)

-- TODO: Switch to Text
scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}
