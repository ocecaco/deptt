{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Deptt.Core.TypeCheck.Monad
  ( TC
  , freshVar
  , withContext
  , lookupType
  , typeError
  , run
  )
where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Reader (ReaderT, MonadReader(..), runReaderT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Deptt.Core.Syntax (Term)
import Deptt.Util.VarSupply (VarSupplyT, runVarSupplyT, fresh)

type TypeError = Text

type Context = Map Text Term

newtype TC a = TC { runTC :: ExceptT TypeError (ReaderT Context (VarSupplyT Identity)) a }
             deriving (Functor, Applicative, Monad)

freshVar :: TC Text
freshVar = TC $ lift . lift $ do
  i <- fresh
  return $ "__typechecker_" <> T.pack (show i)

withContext :: Text -> Term -> TC a -> TC a
withContext name ty (TC act) = TC (local (M.insert name ty) act)

lookupType :: Text -> TC Term
lookupType name = TC $ do
  ctxt <- ask
  case M.lookup name ctxt of
    Just ty -> return ty
    Nothing -> error "lookupType: missing name in environment"

typeError :: TypeError -> TC a
typeError msg = TC (throwError msg)

run :: TC a -> Either Text a
run act = runIdentity (runVarSupplyT (runReaderT (runExceptT (runTC act)) initialContext))
  where initialContext :: Context
        initialContext = M.empty
