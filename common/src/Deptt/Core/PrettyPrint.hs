{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Core.PrettyPrint (prettyPrint) where

import Data.Text.Prettyprint.Doc (Doc, (<>), (<+>), Pretty(..), layoutCompact, space)
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Text (Text)
import Deptt.Core.Syntax (Term(..), Scope(..), Var(..), Name(..), PrettyName(..), Builtin(..), isUnusedScope)
import Control.Monad.Reader (Reader, runReader, MonadReader(..), asks)

-- The operator precedence implementation is based on the "Final
-- Pretty Printer" paper by David Darais, David Christiansen and Weixi
-- Ma as well as the final-pretty-printer package
data NameEnv = NameEnv { prettyNames :: [Text]
                       , level :: Int -- operator precedence level
                       , bumped :: Bool -- whether the current level is bumped (used for associativity)
                       }

newtype PP a = PP { runPP :: Reader NameEnv a }
             deriving (Functor, Applicative, Monad)

initialEnv :: NameEnv
initialEnv = NameEnv [] 0 False

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

withContext :: Text -> PP a -> PP a
withContext name (PP act) = PP $ local updateContext act
  where updateContext :: NameEnv -> NameEnv
        updateContext (NameEnv pnames lvl bmp) = NameEnv (name:pnames) lvl bmp

render :: Term -> PP (Doc a)
render (Var (Bound i)) = pretty <$> lookupName i
render (Var (Free n)) = return (pretty (unwrapPrettyName (prettyName n)))
render (Builtin b) = return (renderBuiltin b)

render (Annotate term ty) = infl 1 (pure " : ") (render term) (render ty)

render (Pi ty s) = do
  (occ, prettyname, prettybody) <- renderScope s
  let prettyty = render ty
  if not occ
  then infr 2 (pure " -> ") prettyty prettybody
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

render (t1 :@ t2) = infl 3 (pure space) (render t1) (render t2)

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
renderScope scope@(ManualScope name body) = do
  let prettyname = unwrapPrettyName name
  let prettybody = withContext prettyname (render body)
  let occ = not (isUnusedScope scope)
  return (occ, pure (pretty prettyname), prettybody)

prettyPrint :: Term -> Text
prettyPrint tm = renderStrict (layoutCompact (runReader (runPP (render tm)) initialEnv))
