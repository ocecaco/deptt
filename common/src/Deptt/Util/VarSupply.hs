{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Deptt.Util.VarSupply (VarSupplyT, runVarSupplyT, fresh) where

import Control.Monad.Trans (MonadTrans)
import Control.Monad.State.Strict (StateT, evalStateT, MonadState(get, put))

newtype VarSupplyT m a = VarSupplyT (StateT Int m a)
                       deriving (Functor, Applicative, Monad, MonadTrans)

fresh :: Monad m => VarSupplyT m Int
fresh = VarSupplyT $ do
  i <- get
  put (i + 1)
  return i

runVarSupplyT :: Monad m => VarSupplyT m a -> m a
runVarSupplyT (VarSupplyT v) = evalStateT v 0
