module Elaborate () where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Data.Text (Text)

-- TODO: Use abstract/instantiate functions to abstract away de Bruijn
-- indices

-- TODO: Implement inductive types based on the Lean implementation
-- (which looks simple enough)

data TermI = Var Int
           | Universe Int
           | App TermI TermC
           | Let Text TermI (Scope TermI)
           | Annotate TermC TermI

           -- dependent product
           | Pi Text TermI (Scope TermI)

          -- -- dependent sum
          -- | Ex Term Term
          -- | ExIntro Term Term
          -- | Fst Term
          -- | Snd Term

          -- -- equality
          -- | Eq Term Term
          -- | Refl
          -- | EqElim Term Term Term Term Term

          -- -- natural numbers
          -- | Nat
          -- | Zero
          -- | Succ Term
          -- | NatElim Term Term Term Term

data TermC = Lambda Text (Scope TermC)

newtype Scope a = Scope a

type Context = [TermI]

newtype TC a = TC { runTC :: ExceptT Text (ReaderT Context Identity) a }
             deriving (Functor, Applicative, Monad)

lookupType :: Int -> TC TermI
lookupType = undefined

inferType :: TermI -> TC TermI
inferType (Var i) = lookupType i -- potentially shift the DeBruijn indices
inferType (Universe k) = Universe (k + 1)
inferType (App f x) = do
  () <- inferPi f
