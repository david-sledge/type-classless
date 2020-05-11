{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.ReaderOp where

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.Trans.ClassOp
import Control.Monad.Trans.Reader (ReaderT(..), Reader, runReader)
import Data.Functor.Identity

type Local m r = forall a . (r -> r) -> m a -> m a

type ReaderF m r = forall a . (r -> a) -> m a

{- class Monad m => Monadreader r m | m -> r where
       ask    :: m r
       local  :: (r -> r) -> m a -> m a
       reader :: (r -> a) -> m a -}
data MonadReaderOp r m = MonadReaderOp {
    _ask              :: m r,
    _local            :: Local m r,
    _reader           :: ReaderF m r,
    _monad'MROp       :: MonadOp m,
    _bind'MR          :: Bind m,
    _applicative'MROp :: ApplicativeOp m,
    _pure'MR          :: Pure m,
    _ap'MR            :: Ap m,
    _functor'MROp     :: FunctorOp m,
    _fmap'MR          :: Fmap m }

{- ask :: (Monad m) => ReaderT r m r
   ask = ReaderT return -}
--ask :: MonadOp m -> ReaderT r m r
ask mOp = ReaderT $ _pure'M mOp

{- local :: (Monad m) =>
       (r -> r) -> ReaderT r m a -> ReaderT r m a
   local f m = ReaderT $ runReaderT m . f -}
--local :: (r' -> r) -> (ReaderT r m) a -> (ReaderT r' m) a
local f m = ReaderT $ runReaderT m . f

{- reader :: (Monad m) => (r -> a) -> ReaderT r m a
   reader f = ReaderT (return . f) -}
--reader :: MonadOp m -> ReaderF (ReaderT r m) r
reader mOp f = ReaderT (_pure'M mOp . f)

{- instance (Monad m) => Monad (ReaderT s m) where
       return = lift . return
       m >>= k = ReaderT $ \r -> do
           a <- runReaderT m r
           runReaderT (k a) r -}
monad'ReaderTOp mOp@(MonadOp { _pure'M = pure, _bind = (>>=)}) = let
    lift = _lift monadTrans'ReaderTOp mOp
  in
  monadOp (lift . pure) $
    \m k -> ReaderT $ \r ->
      runReaderT m r >>= \a ->
      runReaderT (k a) r

--monad'ReaderOp :: MonadOp (ReaderT r Identity)
monad'ReaderOp = monad'ReaderTOp monad'IdentityOp

{- instance MonadTrans ReaderT where
       lift m = ReaderT (const m) -}
--monadTrans'ReaderTOp :: MonadTransOp (ReaderT r)
monadTrans'ReaderTOp = MonadTransOp $ const $ \m -> ReaderT (const m)

{- instance Monad m => MonadReader r (ReaderT r m) where
       ask = ask
       local = local
       reader = reader -}
--monadReader'ReaderTOp :: MonadOp m -> MonadReaderOp r (ReaderT r m )
monadReader'ReaderTOp mOp = let
    monadOp@(MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap
      }) = monad'ReaderTOp mOp
  in
  MonadReaderOp {
    _monad'MROp = monadOp,
    _ask    = ask    mOp,
    _local  = local,
    _reader = reader mOp,
    _bind'MR = bind,
    _applicative'MROp = aOp,
    _pure'MR = pure,
    _ap'MR = ap,
    _functor'MROp = fOp,
    _fmap'MR = fmap }
