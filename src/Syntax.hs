module Syntax (Term(..), Binder(..), substitute, scopeApply, prettyPrint) where

import Data.Maybe (fromMaybe, fromJust)

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Int -- de Bruijn index, also stores the original variable name for pretty printing
          | Universe Int
          | Pi Binder
          | Lambda Binder
          | App Term Term
          deriving (Eq, Ord, Show)

data Binder = Binder (Maybe String) Term Term
           deriving (Ord, Show)

-- names of bound variables are ignored during equality comparison
-- since we are using de Bruijn indices (although the name does get
-- used for pretty printing)
instance Eq Binder where
  Binder _ ty1 body1 == Binder _ ty2 body2 = ty1 == ty2 && body1 == body2

-- whether a variable is shifted depends on whether its index reached
-- outside of the bound variables (into the free variables)

-- this code is based on page 79 of "Types and Programming Languages"
shiftFull :: Int -> Int -> Term -> Term
shiftFull cutoff amount v@(Var k)
  | k >= cutoff = Var (k + amount)
  | otherwise = v

shiftFull _ _ u@(Universe _) = u
shiftFull cutoff amount (Pi s) = Pi (shiftFullBinder cutoff amount s)
shiftFull cutoff amount (Lambda s) = Lambda (shiftFullBinder cutoff amount s)
shiftFull cutoff amount (App t1 t2) = App (shiftFull cutoff amount t1) (shiftFull cutoff amount t2)

shiftFullBinder :: Int -> Int -> Binder -> Binder
shiftFullBinder cutoff amount (Binder name ty body) = Binder name (shiftFull cutoff amount ty) (shiftFull (cutoff + 1) amount body)

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

substituteScope :: Int -> Term -> Binder -> Binder
substituteScope j subst (Binder name ty body) = Binder name (substitute j subst ty) (substitute (j + 1) (shift 1 subst) body)

-- used for beta reduction, takes a scope and substitutes a term into it
scopeApply :: Binder -> Term -> Term
scopeApply (Binder _ _ abstraction) arg = shift (-1) (substitute 0 (shift 1 arg) abstraction)

-- TODO: refactor to use Text instead of String
type NameEnv = [Maybe String]

lookupIndex :: [a] -> Int -> a
lookupIndex = (!!)

count :: Eq a => a -> [a] -> Int
count x xs = length (filter (==x) xs)

envName :: NameEnv -> String -> String
envName env name
  | c == 0 = name
  | otherwise = name ++ show c
  where c = count (Just name) env

prettyPrintHelper :: NameEnv -> Term -> String
-- it shouldn't be possible to refer to a nameless variable with a de
-- Bruijn index, hence the use of fromJust
prettyPrintHelper env (Var i) = fromJust (lookupIndex env i)
prettyPrintHelper _ (Universe k) = "Type " ++ show k
prettyPrintHelper env (Pi s) =
  case maybename of
    Nothing -> "(" ++ prettyty ++ " -> " ++ prettybody ++ ")"
    Just prettyname -> "(forall " ++ prettyname ++ " : " ++ prettyty ++ ", " ++ prettybody ++ ")"
  where (maybename, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (Lambda s) = "(fun " ++ prettyname ++ " : " ++ prettyty ++ " => " ++ prettybody ++ ")"
  where (maybename, prettyty, prettybody) = prettyPrintHelperScope env s
        prettyname = fromMaybe "_" maybename
prettyPrintHelper env (App t1 t2) = prettyPrintHelper env t1 ++ " " ++ prettyPrintHelper env t2

prettyPrintHelperScope :: NameEnv -> Binder -> (Maybe String, String, String)
prettyPrintHelperScope env (Binder rawname ty body) = (prettyname, prettyty, prettybody)
  where prettyname = fmap (envName env) rawname
        prettyty = prettyPrintHelper env ty
        prettybody = prettyPrintHelper (rawname:env) body

prettyPrint :: Term -> String
prettyPrint = prettyPrintHelper []
