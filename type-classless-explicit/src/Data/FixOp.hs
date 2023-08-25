{-# LANGUAGE Rank2Types #-}

module Data.FixOp where

type Mfix m = forall a . (a -> m a) -> m a

{- class (Monad m) => MonadFix m where
    mfix :: (a -> m a) -> m a -}
newtype MfixOp m = MfixOp {_mfix :: Mfix m}

-- mfix :: (?mfixOp::MfixOp m) => Mfix m
-- mfix = _mfix ?mfixOp
