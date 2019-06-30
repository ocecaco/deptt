module Deptt.Core.Normalize.Level (normalizeLevel) where

import Deptt.Core.Syntax (Term(..), Builtin(..))
import Data.List (nub, sort)

-- this normalization code is based on the Agda source
-- code. Specifically, it is based on the levelMax function inside
-- Agda/TypeChecking/Substitute.hs
data ULevel = UZero
            | USucc ULevel
            | UMax ULevel ULevel
            | UNeutral Term

data NPlusLevel = NConst Int
                | NPlus Int Term
                deriving (Eq, Ord)

data NLevel = NMax [NPlusLevel]

translateLevel :: Term -> ULevel
translateLevel (Builtin LevelZero) = UZero
translateLevel (Builtin LevelSucc :@ t) = USucc (translateLevel t)
translateLevel (Builtin LevelMax :@ t1 :@ t2) = UMax (translateLevel t1) (translateLevel t2)
translateLevel t = UNeutral t

pushDownSucc :: ULevel -> NLevel
pushDownSucc UZero = NMax [NConst 0]
pushDownSucc (USucc u) = incrAll (pushDownSucc u)
  where incrAll :: NLevel -> NLevel
        incrAll (NMax lvls) = NMax (map incr lvls)

        incr :: NPlusLevel -> NPlusLevel
        incr (NConst i) = NConst (i + 1)
        incr (NPlus i t) = NPlus (i + 1) t
pushDownSucc (UMax u1 u2) = NMax (n1 ++ n2)
  where NMax n1 = pushDownSucc u1
        NMax n2 = pushDownSucc u2
pushDownSucc (UNeutral t) = NMax [NPlus 0 t]

normalizeNLevel :: NLevel -> NLevel
normalizeNLevel (NMax lvls) = NMax (constant ++ sortedTerms)
  where -- We only need to keep the highest constant in the maximum,
        -- and we don't need to keep a constant k if there is a term of the
        -- form (t + k') for some k' >= k
        largestConstant :: Int
        largestConstant = case cs of
          [] -> 0
          cs' -> maximum cs'
          where cs = [ c | NConst c <- lvls ]

        keepConstant :: Bool
        keepConstant = all (\p -> largestConstant > p) [ p | NPlus p _ <- lvls ] && largestConstant /= 0

        constant :: [NPlusLevel]
        constant
          | keepConstant = [NConst largestConstant]
          | otherwise = []

        -- If there are two terms of the form (t + k) and (t + k'),
        -- then we only need to keep the highest of k and k'
        termsWithIncrements :: [(Term, [Int])]
        termsWithIncrements = [ (t, findIncrements t) | t <- uniqueTerms ]
          where uniqueTerms = nub (sort [ t | NPlus _ t <- lvls ])
                findIncrements t = [ k | NPlus k t' <- lvls, t == t' ]

        highestOnly :: [NPlusLevel]
        highestOnly = [ NPlus (maximum is) t | (t, is) <- termsWithIncrements]

        -- We sort the terms to ensure that ordering of the arguments
        -- to maximum doesn't matter
        sortedTerms :: [NPlusLevel]
        sortedTerms = sort highestOnly

translateNLevel :: NLevel -> Term
translateNLevel (NMax []) = Builtin LevelZero
translateNLevel (NMax ms) = foldr1 (\t1 t2 -> Builtin LevelMax :@ t1 :@ t2) (map translateNPlusLevel ms)

translateNPlusLevel :: NPlusLevel -> Term
translateNPlusLevel (NConst i) = repeatSucc i
  where repeatSucc :: Int -> Term
        repeatSucc 0 = Builtin LevelZero
        repeatSucc n = Builtin LevelSucc :@ repeatSucc (n - 1)

translateNPlusLevel (NPlus k t) = repeatSucc k
  where repeatSucc :: Int -> Term
        repeatSucc 0 = t
        repeatSucc n = Builtin LevelSucc :@ repeatSucc (n - 1)

normalizeLevel :: Term -> Term
normalizeLevel = translateNLevel . normalizeNLevel . pushDownSucc . translateLevel
