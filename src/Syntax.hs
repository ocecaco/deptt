{-# LANGUAGE OverloadedStrings #-}
module Syntax (Term(..), Scope(..), substitute, scopeApply, prettyPrint) where

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Int -- de Bruijn index, also stores the original variable name for pretty printing
          | Universe Int
          | Pi Scope
          | Lambda Scope
          | App Term Term
          deriving (Eq, Ord, Show)

data Scope = Scope String Term Term
           deriving (Eq, Ord, Show)

-- whether a variable is shifted depends on whether its index reached
-- outside of the bound variables (into the free variables)

-- this code is based on page 79 of "Types and Programming Languages"
shiftFull :: Int -> Int -> Term -> Term
shiftFull cutoff amount v@(Var k)
  | k >= cutoff = Var (k + amount)
  | otherwise = v

shiftFull _ _ u@(Universe _) = u
shiftFull cutoff amount (Pi s) = Pi (shiftFullScope cutoff amount s)
shiftFull cutoff amount (Lambda s) = Lambda (shiftFullScope cutoff amount s)
shiftFull cutoff amount (App t1 t2) = App (shiftFull cutoff amount t1) (shiftFull cutoff amount t2)

shiftFullScope :: Int -> Int -> Scope -> Scope
shiftFullScope cutoff amount (Scope name ty body) = Scope name (shiftFull cutoff amount ty) (shiftFull (cutoff + 1) amount body)

shift :: Int -> Term -> Term
shift = shiftFull 0

substitute :: Int -> Term -> Term -> Term
substitute j subst t@(Var k)
  | j == k = subst
  | otherwise = t

substitute _ _ t@(Universe _) = t
substitute j subst (Pi scope) = Pi (substituteScope j subst scope)
substitute j subst (Lambda scope) = Lambda (substituteScope j subst scope)
substitute j subst (App t1 t2) = App (substitute j subst t1) (substitute j subst t2)

substituteScope :: Int -> Term -> Scope -> Scope
substituteScope j subst (Scope name ty body) = Scope name (substitute j subst ty) (substitute (j + 1) (shift 1 subst) body)

-- used for beta reduction, takes a scope and substitutes a term into it
scopeApply :: Scope -> Term -> Term
scopeApply (Scope _ _ abstraction) arg = shift (-1) (substitute 0 (shift 1 arg) abstraction)

-- TODO: refactor to use Text instead of String
type NameEnv = [String]

lookupIndex :: [a] -> Int -> a
lookupIndex = (!!)

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (==x) xs)

envName :: NameEnv -> String -> String
envName env name
  | c == 0 = name
  | otherwise = name ++ show c
  where c = count name env

prettyPrintHelper :: NameEnv -> Term -> String
prettyPrintHelper env (Var i) = lookupIndex env i
prettyPrintHelper _ (Universe k) = "Type " ++ show k
prettyPrintHelper env (Pi s) = "(forall " ++ prettyname ++ " : " ++ prettyty ++ ", " ++ prettybody ++ ")"
  where (prettyname, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (Lambda s) = "(fun " ++ prettyname ++ " : " ++ prettyty ++ " => " ++ prettybody ++ ")"
  where (prettyname, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (App t1 t2) = prettyPrintHelper env t1 ++ " " ++ prettyPrintHelper env t2

prettyPrintHelperScope :: NameEnv -> Scope -> (String, String, String)
prettyPrintHelperScope env (Scope rawname ty body) = (prettyname, prettyty, prettybody)
  where prettyname = envName env rawname
        prettyty = prettyPrintHelper env ty
        prettybody = prettyPrintHelper (rawname:env) body

prettyPrint :: Term -> String
prettyPrint = prettyPrintHelper []
