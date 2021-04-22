{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.ReaderOp where

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.Misc
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
    _monad'MROp       :: MonadOp m }

_bind'MR mrOp = _bind $ _monad'MROp mrOp
_applicative'MROp mrOp = _applicative $ _monad'MROp mrOp
_pure'MR mrOp = _pure'M $ _monad'MROp mrOp
_ap'MR mrOp = _ap'M $ _monad'MROp mrOp
--_functor'MROp mrOp = _functor'MOp $ _monad'MROp mrOp
_fmap'MR mrOp = _fmap'M $ _monad'MROp mrOp

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
monad'ReaderTOp mOp =
  monadOp (readerTLift mOp . _pure'M mOp) $
    \m k -> ReaderT $ \r ->
      _bind mOp (runReaderT m r) $ \a ->
      runReaderT (k a) r

--monad'ReaderOp :: MonadOp (ReaderT r Identity)
monad'ReaderOp = monad'ReaderTOp monad'IdentityOp

{- instance MonadTrans ReaderT where
       lift m = ReaderT (const m) -}
--monadTrans'ReaderTOp :: MonadTransOp (ReaderT r)
readerTLift = const $ \m -> ReaderT (const m)

{- instance Monad m => MonadReader r (ReaderT r m) where
       ask = ask
       local = local
       reader = reader -}
--monadReader'ReaderTOp :: MonadOp m -> MonadReaderOp r (ReaderT r m )
monadReader'ReaderTOp mOp =
  MonadReaderOp {
    _monad'MROp = monad'ReaderTOp mOp,
    _ask = ask mOp,
    _local = local,
    _reader = reader mOp }
