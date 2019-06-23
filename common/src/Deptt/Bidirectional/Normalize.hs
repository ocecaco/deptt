{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Bidirectional.Normalize (normalizeTerm) where

import Deptt.Bidirectional.Syntax (TermI(..), TermC(..), Scope, Var(..), abstractC, abstractI, instantiateI, instantiateC)
import Control.Monad.Identity (Identity)
import Data.Text (Text)

-- Problem: What if we have a place that requires an inferable term,
-- but in the process of normalizing it, the subterm that was there
-- turns into a term that is only checkable? We would have to add a
-- type annotation to it?

newtype Norm a = Norm { runNorm :: Identity a }
               deriving (Functor, Applicative, Monad)

freshVar :: Norm Text
freshVar = undefined

normalizeTerm :: TermC -> TermC
normalizeTerm = undefined
