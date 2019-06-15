module PrettyPrint (prettyPrint) where

import Syntax (Term(..), Binder(..), Builtin(..))

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
prettyPrintHelper env (Let def s) = "(let " ++ prettyname ++ " : " ++ prettyty ++ " = " ++ prettyPrintHelper env def ++ " in " ++ prettybody ++ ")"
  where (_, prettyname, prettyty, prettybody) = prettyPrintHelperScope env s
prettyPrintHelper env (App t1 t2) = "(" ++ prettyPrintHelper env t1 ++ " " ++ prettyPrintHelper env t2 ++ ")"
prettyPrintHelper _ (Builtin b) = prettyPrintBuiltin b

prettyPrintBuiltin :: Builtin -> String
prettyPrintBuiltin Nat = "nat"
prettyPrintBuiltin Zero = "zero"
prettyPrintBuiltin Succ = "succ"
prettyPrintBuiltin NatElim = "natelim"
prettyPrintBuiltin Eq = "eq"
prettyPrintBuiltin Refl = "refl"
prettyPrintBuiltin EqElim = "eqelim"

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
occursVar k (Let def (Binder _ t1 t2)) = occursVar k def || occursVar k t1 || occursVar (k + 1) t2
occursVar k (App t1 t2) = occursVar k t1 || occursVar k t2
occursVar _ (Universe _) = False
occursVar _ (Builtin _) = False
