{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Trans.ContOps where

import Prelude hiding (return, (>>=))
import Control.MonadOps
import Control.Monad.Cont (ContT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Trans.ReaderOps
import Control.Monad.Trans.StateOps
import Control.Monad.State.Strict (StateT(..))

{- instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c) -}

contTMonadOps :: MonadOps (ContT r m)
contTMonadOps = pkgMonadOps (\x -> ContT ($ x)) $
    \m k -> ContT $ \ c -> runContT m $ flip runContT c . k

{- instance MonadTrans (ContT r) where
       lift m = ContT (m >>=) -}
contTLiftOp :: LiftOp (ContT r)
contTLiftOp = LiftOp $ ContT . (>>=)

{- evalContT :: (Monad m) => ContT r m r -> m r
   evalContT m = runContT m return -}
evalContT :: (?monadOps::MonadOps m) => ContT r m r -> m r
evalContT m = runContT m $ _pure monadApplicativeOps

mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

withContT :: ((a1 -> m r) -> a2 -> m r) -> ContT r m a2 -> ContT r m a1
withContT f m = ContT $ runContT m . f

type CallCC m = forall a b . ((a -> m b) -> m a) -> m a

callCC :: CallCC (ContT r m)
callCC f = ContT $ \ c -> runContT (f $ \ x -> ContT . const $ c x) c

{- resetT :: (Monad m) => ContT r m r -> ContT r' m r
   resetT = lift . evalContT -}
resetT :: (?monadOps::MonadOps m) => ContT a m a -> ContT r m a
resetT = _lift contTLiftOp . evalContT

{- shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
   shiftT f = ContT (evalContT . f) -}
shiftT :: (?monadOps::MonadOps m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (evalContT . f)

{- instance MonadReader r' m => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local
    reader = lift . reader -}
contTMonadReaderOps :: (?monadReaderOps :: MonadReaderOps r' m) => MonadReaderOps r' (ContT r m)
contTMonadReaderOps =
  let ?monadOps = monadReaderMonadOps in
  let ?liftOp = contTLiftOp in
  MonadReaderOps
    contTMonadOps
    (lift ask)
    (\f m -> ContT $ local f . runContT m)
    (lift . reader)

{- instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state -}
contTMonadStateOps :: (?monadStateOps :: MonadStateOps s m) => MonadStateOps s (ContT r m)
contTMonadStateOps =
  let ?monadOps = monadStateMonadOps in
  let ?liftOp = contTLiftOp in
  MonadStateOps contTMonadOps (lift . state)

callCCReaderT :: CallCC m -> CallCC (ReaderT r m)
callCCReaderT callCCF f = ReaderT $ \ r ->
    callCCF $ \ c ->
    runReaderT (f (ReaderT . const . c)) r

{- liftCallCC'State'
  :: ((((a,s) -> m (b,s)) -> m (a,s)) -> m (a,s))
    -> ((a -> (StateT s m) b) -> (StateT s m) a)
    -> (StateT s m) a -}
callCCStateT :: CallCC m -> CallCC (StateT s m)
callCCStateT callCCF f = StateT $ \ s ->
    callCCF $ \ c ->
    runStateT (f (\ a -> StateT $ \ s' -> c (a, s'))) s
