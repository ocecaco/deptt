module Syntax (Term(..), Binder(..), Builtin(..), substitute, scopeApply, prettyPrint, shift) where

-- TODO: Add credits to Andrej Bauer for some of the code that is
-- similar
data Term = Var Int -- de Bruijn index, also stores the original variable name for pretty printing
          | Pi Binder
          | Lambda Binder
          | App Term Term

          -- predicative hierarchy of universes
          | Universe Int
          | Builtin Builtin
          -- TODO: unit, sum, product, dependent sum (sigma)?
          deriving (Eq, Ord, Show)

data Binder = Binder String Term Term
           deriving (Ord, Show)

data Builtin = Nat | Zero | Succ | NatElim
             deriving (Eq, Ord, Show)

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

shiftFull cutoff amount (Pi s) = Pi (shiftFullBinder cutoff amount s)
shiftFull cutoff amount (Lambda s) = Lambda (shiftFullBinder cutoff amount s)
shiftFull cutoff amount (App t1 t2) = App (shiftFull cutoff amount t1) (shiftFull cutoff amount t2)

shiftFull _ _ t = t

shiftFullBinder :: Int -> Int -> Binder -> Binder
shiftFullBinder cutoff amount (Binder name ty body) = Binder name (shiftFull cutoff amount ty) (shiftFull (cutoff + 1) amount body)

shift :: Int -> Term -> Term
shift = shiftFull 0

substitute :: Int -> Term -> Term -> Term
substitute j subst t@(Var k)
  | j == k = subst
  | otherwise = t

substitute j subst (Pi scope) = Pi (substituteScope j subst scope)
substitute j subst (Lambda scope) = Lambda (substituteScope j subst scope)
substitute j subst (App t1 t2) = App (substitute j subst t1) (substitute j subst t2)

substitute _ _ t = t

substituteScope :: Int -> Term -> Binder -> Binder
substituteScope j subst (Binder name ty body) = Binder name (substitute j subst ty) (substitute (j + 1) (shift 1 subst) body)

-- used for beta reduction, takes a scope and substitutes a term into it
scopeApply :: Binder -> Term -> Term
scopeApply (Binder _ _ abstraction) arg = shift (-1) (substitute 0 (shift 1 arg) abstraction)

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
prettyPrintHelper env (Pi s) =
  if not occ
  then "(" ++ prettyty ++ " -> " ++ prettybody ++ ")"
  else "(forall " ++ prettyname ++ " : " ++ prettyty ++ ", " ++ prettybody ++ ")"
  where (occ, prettyname, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (Lambda s) = "(fun " ++ prettyname ++ " : " ++ prettyty ++ " => " ++ prettybody ++ ")"
  where (_, prettyname, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (App t1 t2) = "(" ++ prettyPrintHelper env t1 ++ " " ++ prettyPrintHelper env t2 ++ ")"
prettyPrintHelper _ (Builtin b) = prettyPrintBuiltin b

prettyPrintBuiltin :: Builtin -> String
prettyPrintBuiltin Nat = "nat"
prettyPrintBuiltin Zero = "zero"
prettyPrintBuiltin Succ = "succ"
prettyPrintBuiltin NatElim = "natelim"

prettyPrintHelperScope :: NameEnv -> Binder -> (Bool, String, String, String)
prettyPrintHelperScope env (Binder rawname ty body) = (occ, prettyname, prettyty, prettybody)
  where prettyname = envName env rawname
        prettyty = prettyPrintHelper env ty
        prettybody = prettyPrintHelper (rawname:env) body
        occ = occursVar 0 body

prettyPrint :: Term -> String
prettyPrint = prettyPrintHelper []

occursVar :: Int -> Term -> Bool
occursVar k (Var i) = k == i
occursVar k (Pi (Binder _ t1 t2)) = occursVar k t1 || occursVar (k + 1) t2
occursVar k (Lambda (Binder _ t1 t2)) = occursVar k t1 || occursVar (k + 1) t2
occursVar k (App t1 t2) = occursVar k t1 || occursVar k t2
occursVar _ (Universe _) = False
occursVar _ (Builtin _) = False
