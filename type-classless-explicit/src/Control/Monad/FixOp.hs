{-# LANGUAGE Rank2Types #-}

module Control.Monad.FixOp where

import Control.MonadOp

type Mfix m = forall a . (a -> m a) -> m a

{- class (Monad m) => MonadFix m where
       mfix :: (a -> m a) -> m a -}
data MonadFixOp m = MonadFixOp
  { _mfix             :: Mfix m
  , _monad'MFOp       :: MonadOp m
  }

_bind'MF mfOp = _bind $ _monad'MFOp mfOp
_applicative'MFOp mfOp = _applicative $ _monad'MFOp mfOp
_pure'MF mfOp = _pure'M $ _monad'MFOp mfOp
_ap'MF mfOp = _ap'M $ _monad'MFOp mfOp
_fmap'MF mfOp = _fmap'M $ _monad'MFOp mfOp

monadFixOp :: Pure m -> Bind m -> Mfix m -> MonadFixOp m
monadFixOp pure bind mfix =
  MonadFixOp
  { _mfix = mfix
  , _monad'MFOp = monadOp pure bind
  }
