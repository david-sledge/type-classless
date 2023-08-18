{-# LANGUAGE Rank2Types #-}

module Data.FixOp where

-- MonadFix without the Monad constraint
newtype MfixOp m = MfixOp (forall a . (a -> m a) -> m a)
