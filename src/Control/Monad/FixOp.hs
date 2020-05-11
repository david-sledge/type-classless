{-# LANGUAGE Rank2Types #-}

module Control.Monad.FixOp where

import Control.MonadOp
import Data.Functor.Identity
import Data.Function ( fix )
import System.IO

type Mfix m = forall a . (a -> m a) -> m a

{- class (Monad m) => MonadFix m where
       mfix :: (a -> m a) -> m a -}
data MonadFixOp m = MonadFixOp {
    _mfix             :: Mfix m,
    _monad'MFOp       :: MonadOp m,
    _bind'MF          :: Bind m,
    _applicative'MFOp :: ApplicativeOp m,
    _pure'MF          :: Pure m,
    _ap'MF            :: Ap m,
    _functor'MFOp     :: FunctorOp m,
    _fmap'MF          :: Fmap m }

monadFixOp :: Pure m -> Bind m -> Mfix m -> MonadFixOp m
monadFixOp pure bind mfix = let
    mOp@(MonadOp {
          _applicative'MOp = aOp,
          _ap'M = ap,
          _functor'MOp = fOp,
          _fmap'M = fmap
        }) = monadOp pure bind
  in
  MonadFixOp {
    _mfix = mfix,
    _monad'MFOp = mOp,
    _bind'MF = bind,
    _applicative'MFOp = aOp,
    _pure'MF = pure,
    _ap'MF = ap,
    _functor'MFOp = fOp,
    _fmap'MF = fmap }

--------------------------------------------------------------------------------
monadFix'IOOp    :: MonadFixOp    IO
monad'IOOp       :: MonadOp       IO
applicative'IOOp :: ApplicativeOp IO
functor'IOOp     :: FunctorOp     IO
monadFix'IOOp@(MonadFixOp {
    _monad'MFOp = monad'IOOp,
    _applicative'MFOp = applicative'IOOp,
    _functor'MFOp = functor'IOOp }) =
  monadFixOp return (>>=) fixIO

monadFix'ListOp    :: MonadFixOp    []
monad'ListOp       :: MonadOp       []
applicative'ListOp :: ApplicativeOp []
functor'ListOp     :: FunctorOp     []
monadFix'ListOp@(MonadFixOp {
    _monad'MFOp = monad'ListOp,
    _applicative'MFOp = applicative'ListOp,
    _functor'MFOp = functor'ListOp }) =
  monadFixOp return (>>=) $ fix (\mfix f ->
      case fix (f . head) of
      []    -> []
      (x:_) -> x : mfix (tail . f))

{- instance Monad (Identity) where
       return = Identity
       m >>= k = k $ runIdentity m -}
monadFix'IdentityOp    :: MonadFixOp    Identity
monad'IdentityOp       :: MonadOp       Identity
applicative'IdentityOp :: ApplicativeOp Identity
functor'IdentityOp     :: FunctorOp     Identity
monadFix'IdentityOp@(MonadFixOp {
    _monad'MFOp = monad'IdentityOp,
    _applicative'MFOp = applicative'IdentityOp,
    _functor'MFOp = functor'IdentityOp }) =
  monadFixOp Identity (\m k -> k $ runIdentity m) $ \f ->
    Identity . fix $ runIdentity . f
