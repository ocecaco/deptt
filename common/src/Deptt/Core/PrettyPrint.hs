{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Core.PrettyPrint (prettyPrint, prettyPrintWithContext) where

import Data.Text.Prettyprint.Doc (Doc, (<>), (<+>), Pretty(..), layoutCompact, space)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Deptt.Core.Syntax (Term(..), Scope(..), Var(..), Name(..), PrettyName(..), Builtin(..))
import qualified Data.Set as S
import Data.Set (Set)
import Control.Monad.Reader (Reader, runReader, MonadReader(..), asks)

-- The operator precedence implementation is based on the "Final
-- Pretty Printer" paper by David Darais, David Christiansen and Weixi
-- Ma as well as the final-pretty-printer package
data NameEnv = NameEnv { usedNames :: Set Text
                       , prettyNames :: [Text]
                       , level :: Int -- operator precedence level
                       , bumped :: Bool -- whether the current level is bumped (used for associativity)
                       }

newtype PP a = PP { runPP :: Reader NameEnv a }
             deriving (Functor, Applicative, Monad)

initialEnv :: NameEnv
initialEnv = NameEnv S.empty [] 0 False

-- The following functions are mostly based on the
-- final-pretty-printer package

askLevel :: PP Int
askLevel = PP (asks level)

withLevel :: Int -> PP a -> PP a
withLevel lvl (PP act) = PP (local (\env -> env { level = lvl }) act)

withBumped :: Bool -> PP a -> PP a
withBumped bmp (PP act) = PP (local (\env -> env { bumped = bmp }) act)

askBumped :: PP Bool
askBumped = PP (asks bumped)

parens :: PP (Doc a) -> PP (Doc a)
parens sub = do
  sub' <- withLevel 0 $ withBumped False sub
  return ("(" <> sub' <> ")")

-- switch to a different precedence level for the given computation,
-- adding parentheses when necessary
atLevel :: Int -> PP (Doc a) -> PP (Doc a)
atLevel newLevel act = do
  oldLevel <- askLevel
  oldBumped <- askBumped
  let act' = withLevel newLevel (withBumped False act)
  if oldLevel < newLevel || (oldLevel == newLevel && not oldBumped)
    then act'
    else parens act'

-- Bump the precedence level for associativity
bump :: PP a -> PP a
bump = withBumped True

infl :: Int -> PP (Doc a) -> PP (Doc a) -> PP (Doc a) -> PP (Doc a)
infl lvl op x1 x2 = atLevel lvl $ do
  op' <- op
  x1' <- x1
  x2' <- bump x2
  return (x1' <> op' <> x2')

infr :: Int -> PP (Doc a) -> PP (Doc a) -> PP (Doc a) -> PP (Doc a)
infr lvl op x1 x2 = atLevel lvl $ do
  op' <- op
  x1' <- bump x1
  x2' <- x2
  return (x1' <> op' <> x2')

lookupName :: Int -> PP Text
lookupName i = PP $ do
  env <- ask
  return (prettyNames env !! i)

disambiguate :: Text -> PP Text
disambiguate name = PP $ do
  env <- ask
  return (findUnique (usedNames env))
  where findUnique :: Set Text -> Text
        findUnique used = go 0
          where go :: Int -> Text
                go n
                  | not (newname `S.member` used) = newname
                  | otherwise = go (n + 1)
                  where newname
                          | n == 0 = name
                          | otherwise = name <> T.pack (show n)

withContext :: Text -> PP a -> PP a
withContext name (PP act) = PP $ local updateContext act
  where updateContext :: NameEnv -> NameEnv
        updateContext (NameEnv used pnames lvl bmp) = NameEnv (S.insert name used) (name:pnames) lvl bmp

render :: Term -> PP (Doc a)
render (Var (Bound i)) = pretty <$> lookupName i
render (Var (Free n)) = return (pretty (unwrapPrettyName (prettyName n)))
render (Builtin b) = return (renderBuiltin b)

render (Pi ty s) = do
  (occ, prettyname, prettybody) <- renderScope s
  let prettyty = render ty
  if not occ
  then infr 1 (pure " -> ") prettyty prettybody
  else atLevel 0 $ do
    prettyname' <- prettyname
    prettyty' <- prettyty
    prettybody' <- prettybody
    return $ "forall" <+> prettyname' <+> ":" <+> prettyty' <> "," <+> prettybody'

render (Lambda ty s) = atLevel 0 $ do
  (_, prettyname, prettybody) <- renderScope s
  let prettyty = render ty
  prettyname' <- prettyname
  prettyty' <- prettyty
  prettybody' <- prettybody
  return $ "fun" <+> prettyname' <+> ":" <+> prettyty' <+> "=>" <+> prettybody'

render (Let ty def s) = atLevel 0 $ do
  (_, prettyname, prettybody) <- renderScope s
  rdef <- render def
  prettyname' <- prettyname
  prettyty' <- render ty
  prettybody' <- prettybody
  return $ "let" <+> prettyname' <+> ":" <+> prettyty' <+> "=" <+> rdef <+> "in" <+> prettybody'

render (t1 :@ t2) = infl 2 (pure space) (render t1) (render t2)

renderBuiltin :: Builtin -> Doc a
renderBuiltin Nat = "nat"
renderBuiltin Zero = "zero"
renderBuiltin Succ = "succ"
renderBuiltin NatElim = "natelim"
renderBuiltin Eq = "eq"
renderBuiltin Refl = "refl"
renderBuiltin EqElim = "eqelim"
renderBuiltin Ex = "ex"
renderBuiltin Pack = "pack"
renderBuiltin Fst = "fst"
renderBuiltin Snd = "snd"
renderBuiltin Or = "or"
renderBuiltin InL = "inl"
renderBuiltin InR = "inr"
renderBuiltin OrElim = "orelim"
renderBuiltin List = "list"
renderBuiltin Nil = "nil"
renderBuiltin Cons = "cons"
renderBuiltin ListElim = "listelim"
renderBuiltin And = "and"
renderBuiltin Pair = "pair"
renderBuiltin Proj1 = "proj1"
renderBuiltin Proj2 = "proj2"
renderBuiltin Unit = "unit"
renderBuiltin Tt = "tt"
renderBuiltin UnitElim = "unitelim"
renderBuiltin Void = "void"
renderBuiltin VoidElim = "voidelim"
renderBuiltin Level = "level"
renderBuiltin LevelZero = "lzero"
renderBuiltin LevelSucc = "lsucc"
renderBuiltin LevelMax = "lmax"
renderBuiltin UniverseTop = "typeomega"
renderBuiltin Universe = "type"

renderScope :: Scope -> PP (Bool, PP (Doc a), PP (Doc a))
renderScope (ManualScope name body) = do
  let prettyname = unwrapPrettyName name
  let prettybody = withContext prettyname (render body)
  let occ = occursVar 0 body
  return (occ, pure (pretty prettyname), prettybody)

prettyPrint :: Term -> Text
prettyPrint = prettyPrintWithContext []

prettyPrintWithContext :: [Text] -> Term -> Text
prettyPrintWithContext env tm = renderStrict (layoutCompact (runReader (runPP (disambiguateContext env (render tm))) initialEnv))
  where disambiguateContext :: [Text] -> PP a -> PP a
        disambiguateContext [] act = act
        disambiguateContext (n:ns) act = do
          prettyname <- disambiguate n
          withContext prettyname (disambiguateContext ns act)

occursVar :: Int -> Term -> Bool
occursVar k (Var (Bound i)) = k == i
occursVar k (Pi ty scope) = occursVar k ty || occursVarScope k scope
occursVar k (Lambda ty scope) = occursVar k ty || occursVarScope k scope
occursVar k (Let ty def scope) = occursVar k ty || occursVar k def || occursVarScope k scope
occursVar k (t1 :@ t2) = occursVar k t1 || occursVar k t2
occursVar _ (Builtin _) = False
occursVar _ (Var (Free _)) = False

occursVarScope :: Int -> Scope -> Bool
occursVarScope i (ManualScope _ body) = occursVar (i + 1) body
