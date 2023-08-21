{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Trans.ContOps where

import Prelude hiding (return, (>>=))
import Control.MonadOps
import Control.Monad.Cont (ContT(..), Cont, cont, runCont)
import Control.Monad.Reader (ReaderT(..), Reader, runReader)
import Control.Monad.Trans.ReaderOps
import Control.Monad.Trans.StateOps
import Control.Monad.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Data.Functor.Identity

{- instance MonadTrans (ContT r) where
       lift m = ContT (m >>=) -}
contTMonadTransOp :: (?monadOps::MonadOps m) => m a -> ContT r m a
contTMonadTransOp m = ContT (m >>=)

{- instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c) -}
--contTMonadOps :: MonadOp (ContT r m)
contTMonadOps :: MonadOps (ContT r m)
contTMonadOps = pkgMonadOps (\x -> ContT ($ x)) $
    \m k -> ContT $ \ c -> runContT m (\ x -> runContT (k x) c)

{- evalContT :: (Monad m) => ContT r m r -> m r
   evalContT m = runContT m return -}
--evalContT :: MonadOp m -> ContT r m r -> m r
evalContT :: (?monadOps::MonadOps m) => ContT r m r -> m r
evalContT m = runContT m return

--mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m = ContT $ f . runContT m

--withContT :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = ContT $ runContT m . f

--callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
--callCC :: CallCC (ContT r m)
callCC f = ContT $ \ c -> runContT (f $ \ x -> ContT . const $ c x) c

{- resetT :: (Monad m) => ContT r m r -> ContT r' m r
   resetT = lift . evalContT -}
--resetT :: MonadOp m -> ContT r m r -> ContT r' m r
resetT :: (?monadOps::MonadOps m) => ContT a m a -> ContT r m a
resetT = contTMonadTransOp . evalContT

{- shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
   shiftT f = ContT (evalContT . f) -}
--shiftT :: MonadOp m -> ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT :: (?monadOps::MonadOps m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT (evalContT . f)

{- instance MonadReader r' m => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local
    reader = lift . reader -}
--contTMonadReaderOps :: MonadReaderOp r' m -> MonadReaderOp r' (ContT r m)
-- contTMonadReaderOps mrOps = let
--     lift = _lift contTMonadTransOp $ _monadReaderMonadOps mrOps
--   in
--   MonadReaderOps
--     (lift $ _ask mrOps)
--     (\f m -> ContT $ _local mrOps f . runContT m)
--     (lift . _reader mrOps)
--     contTMonadOps

{- instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state -}
--contTMonadStateOps :: MonadStateOp s m -> MonadStateOp s (ContT r m)
-- contTMonadStateOps msOp = let
--     lift = _lift contTMonadTransOp $ _monadStateMonadOps msOp
--   in MonadStateOps (\f -> lift . _modifyAnd msOp f)
--     (lift $ _get msOp)
--     (lift . _state msOp)
--     contTMonadOps

--liftCallCC'Reader :: CallCC m -> CallCC (ReaderT r m)
-- liftCallCC'Reader callCC f = ReaderT $ \ r ->
--     callCC $ \ c ->
--     runReaderT (f (ReaderT . const . c)) r

{- liftCallCC'State'
  :: ((((a,s) -> m (b,s)) -> m (a,s)) -> m (a,s))
    -> ((a -> (StateT s m) b) -> (StateT s m) a)
    -> (StateT s m) a -}
--liftCallCC'State' :: CallCC m -> CallCC (StateT s m)
-- liftCallCC'State' callCC f = StateT $ \ s ->
--     callCC $ \ c ->
--     runStateT (f (\ a -> StateT $ \ s' -> c (a, s'))) s
