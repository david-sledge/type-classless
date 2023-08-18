{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Trans.ReaderOps where

import Prelude hiding (return, (>>=))
import Control.MonadOps
import Control.Monad.Misc
import Control.Monad.Reader (ReaderT(..))
import Data.Functor.Identity

type Local m r = forall a . (r -> r) -> m a -> m a

type ReaderF m r = forall a . (r -> a) -> m a

{- class Monad m => Monadreader r m | m -> r where
       ask    :: m r
       local  :: (r -> r) -> m a -> m a
       reader :: (r -> a) -> m a -}
data MonadReaderOps r m = MonadReaderOps
  { _ask                  :: m r
  , _local                :: Local m r
  , _reader               :: ReaderF m r
  , _monadReaderMonadOps  :: MonadOps m
  }

monadReaderMonadOps :: (?monadReaderOps :: MonadReaderOps r m) => MonadOps m
monadReaderMonadOps = _monadReaderMonadOps ?monadReaderOps

monadReaderApplicativeOps :: (?monadReaderOps :: MonadReaderOps r m) => ApplicativeOps m
monadReaderApplicativeOps = _applicativeOps monadReaderMonadOps

monadReaderPure :: (?monadReaderOps :: MonadReaderOps r m) => Pure m
monadReaderPure = _pure monadReaderApplicativeOps

monadReaderAp :: (?monadReaderOps :: MonadReaderOps r m) => Ap m
monadReaderAp = _ap monadReaderApplicativeOps

monadReaderFmap :: (?monadReaderOps :: MonadReaderOps r m) => Fmap m
monadReaderFmap =
  let ?applicativeOps = monadReaderApplicativeOps in applicativeFmap

{- ask :: (Monad m) => ReaderT r m r
   ask = ReaderT return -}
--ask :: MonadOp m -> ReaderT r m r
ask :: (?monadOps :: MonadOps m) => ReaderT r m r
ask = ReaderT return

{- local :: (Monad m) =>
       (r -> r) -> ReaderT r m a -> ReaderT r m a
   local f m = ReaderT $ runReaderT m . f -}
local :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
local f m = ReaderT $ runReaderT m . f

{- reader :: (Monad m) => (r -> a) -> ReaderT r m a
   reader f = ReaderT (return . f) -}
--reader :: MonadOp m -> ReaderF (ReaderT r m) r
reader :: (?monadOps::MonadOps m) => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

{- instance (Monad m) => Monad (ReaderT s m) where
       return = lift . return
       m >>= k = ReaderT $ \r -> do
           a <- runReaderT m r
           runReaderT (k a) r -}
readerTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (ReaderT r m)
readerTMonadOps =
  monadOps (readerTLift . return) $
    \m k -> ReaderT $ \r ->
      (>>=) (runReaderT m r) $ \a ->
      runReaderT (k a) r

readerMonadOps :: MonadOps (ReaderT r Identity)
readerMonadOps = let ?monadOps = identityMonadOps in readerTMonadOps

{- instance MonadTrans ReaderT where
       lift m = ReaderT (const m) -}
--monadTrans'ReaderTOp :: MonadTransOp (ReaderT r)
readerTLift :: m a -> ReaderT r m a
readerTLift = ReaderT . const

{- instance Monad m => MonadReader r (ReaderT r m) where
       ask = ask
       local = local
       reader = reader -}
--monadReader'ReaderTOp :: MonadOp m -> MonadReaderOp r (ReaderT r m )
readerTMonadReaderOps :: (?monadOps :: MonadOps m) => MonadReaderOps r (ReaderT r m)
readerTMonadReaderOps =
  MonadReaderOps ask local reader readerTMonadOps
