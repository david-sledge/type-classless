{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Trans.ReaderOps where

import Prelude hiding (return, (>>=), pure)
import Control.MonadOps
import Control.Monad.Misc
import Control.Monad.Reader (ReaderT(..))
import Data.Functor.Identity

{- instance MonadTrans ReaderT where
    lift m = ReaderT (const m) -}
readerTLiftOp :: LiftOp (ReaderT r)
readerTLiftOp = LiftOp $ ReaderT . const

{- instance (Monad m) => Monad (ReaderT s m) where
    return = lift . return
    m >>= k = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (k a) r -}
readerTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (ReaderT r m)
readerTMonadOps =
  let ?applicativeOps = monadApplicativeOps
      ?liftOp = readerTLiftOp
  in
  pkgMonadOps (lift . pure) $
    \m k -> ReaderT $ \r ->
      (>>=) (runReaderT m r) $ \a ->
      runReaderT (k a) r

{- class Monad m => Monadreader r m | m -> r where
    ask    :: m r
    local  :: (r -> r) -> m a -> m a
    reader :: (r -> a) -> m a -}
type Local m r = forall a . (r -> r) -> m a -> m a
type ReaderF m r = forall a . (r -> a) -> m a

data MonadReaderOps r m = MonadReaderOps
  {
    _monadReaderMonadOps  :: MonadOps m,
    _ask                  :: m r,
    _local                :: Local m r,
    _reader               :: ReaderF m r }

ask :: (?monadReaderOps :: MonadReaderOps r m) => m r
ask = _ask ?monadReaderOps

local :: (?monadReaderOps :: MonadReaderOps r m) => Local m r
local = _local ?monadReaderOps

reader :: (?monadReaderOps :: MonadReaderOps r m) => ReaderF m r
reader = _reader ?monadReaderOps

monadReaderMonadOps :: (?monadReaderOps :: MonadReaderOps r m) => MonadOps m
monadReaderMonadOps = _monadReaderMonadOps ?monadReaderOps

pkgMonadReaderOps :: (?monadOps::MonadOps m) => Local m r -> Either (m r, ReaderF m r) (Either (m r) (ReaderF m r)) -> MonadReaderOps r m
pkgMonadReaderOps localF (Left (askF, readerF)) =
  MonadReaderOps ?monadOps askF localF readerF
pkgMonadReaderOps localF (Right (Left askF)) =
  let ?functorOps = _applicativeFunctorOps monadApplicativeOps in
{- reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r) -}
  MonadReaderOps ?monadOps askF localF (askF <&>)
pkgMonadReaderOps localF (Right (Right readerF)) =
{- ask   :: m r
    ask = reader id -}
  MonadReaderOps ?monadOps (readerF id) localF readerF

withReaderT :: (r -> r') -> ReaderT r' m a -> ReaderT r m a
withReaderT f m = ReaderT $ runReaderT m . f

readerTMonadReaderOps :: (?monadOps :: MonadOps m) => MonadReaderOps r (ReaderT r m)
readerTMonadReaderOps =
  let ?applicativeOps = monadApplicativeOps in
  let ?monadOps = readerTMonadOps in
  pkgMonadReaderOps
{- local :: (Monad m) => (r -> r) -> ReaderT r m a -> ReaderT r m a
   local f m = ReaderT $ runReaderT m . f -}
    withReaderT $ Left (
{- ask :: (Monad m) => ReaderT r m r
   ask = ReaderT return -}
      ReaderT pure,
{- reader :: (Monad m) => (r -> a) -> ReaderT r m a
   reader f = ReaderT (return . f) -}
      ReaderT . (pure .))

readerMonadOps :: MonadOps (ReaderT r Identity)
readerMonadOps = let ?monadOps = identityMonadOps in readerTMonadOps
