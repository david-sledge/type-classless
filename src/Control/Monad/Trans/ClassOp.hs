{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.ClassOp where

import Control.MonadOp

type Lift t = forall a m . MonadOp m -> m a -> t m a

{- class MonadTrans t where
       lift :: Monad m => m a -> t m a -}
newtype MonadTransOp t = MonadTransOp {
    _lift :: Lift t }
