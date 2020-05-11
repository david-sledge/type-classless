{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.ContOp where

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.Trans.ClassOp
import Control.Monad.Trans.Cont (ContT(..), Cont, cont, runCont)
import Control.Monad.Trans.Reader (ReaderT(..), Reader, runReader)
import Control.Monad.Trans.ReaderOp
import Control.Monad.Trans.StateOp
import Control.Monad.Trans.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Data.Functor.Identity

type CallCC m = forall a b . ((a -> m b) -> m a) -> m a

newtype CallCCOp m = CallCCOp { _callCC :: CallCC m }

{- instance MonadTrans (ContT r) where
       lift m = ContT (m >>=) -}
--monadTrans'ContTOp :: MonadTransOp (ContT r)
monadTrans'ContTOp = MonadTransOp $
  \(MonadOp { _bind = (>>=)}) m -> ContT (m >>=)

{- instance Monad (ContT r m) where
    return x = ContT ($ x)
    m >>= k  = ContT $ \ c -> runContT m (\ x -> runContT (k x) c) -}
--monad'ContTOp :: MonadOp (ContT r m)
monad'ContTOp = monadOp (\x -> ContT ($ x)) $
    \m k -> ContT $ \ c -> runContT m (\ x -> runContT (k x) c)

{- evalContT :: (Monad m) => ContT r m r -> m r
   evalContT m = runContT m return -}
--evalContT :: MonadOp m -> ContT r m r -> m r
evalContT (MonadOp { _pure'M = pure }) m = runContT m pure

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
resetT mOp = (_lift monadTrans'ContTOp mOp) . evalContT mOp

{- shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
   shiftT f = ContT (evalContT . f) -}
--shiftT :: MonadOp m -> ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT mOp f = ContT (evalContT mOp . f)

{- instance MonadReader r' m => MonadReader r' (ContT r m) where
    ask   = lift ask
    local = Cont.liftLocal ask local
    reader = lift . reader -}
--monadReader'ContTOp :: MonadReaderOp r' m -> MonadReaderOp r' (ContT r m)
monadReader'ContTOp (MonadReaderOp {
    _monad'MROp = mOp,
    _ask = ask,
    _local = local,
    _reader = reader }) = let
    lift = _lift monadTrans'ContTOp mOp
    MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap } = monad'ContTOp
  in
  MonadReaderOp {
    _monad'MROp = monad'ContTOp,
    _ask = lift ask,
    _local = \f m -> ContT $ local f . runContT m,
    _reader = lift . reader,
    _bind'MR = bind,
    _applicative'MROp = aOp,
    _pure'MR = pure,
    _ap'MR = ap,
    _functor'MROp = fOp,
    _fmap'MR = fmap }

{- instance MonadState s m => MonadState s (ContT r m) where
    get = lift get
    put = lift . put
    state = lift . state -}
--monadState'ContTOp :: MonadStateOp s m -> MonadStateOp s (ContT r m)
monadState'ContTOp (MonadStateOp {
        _monad'MSOp = mOp,
        _modifyAnd = modifyAnd,
        _get = get,
        _state = state
      }) = let
    lift = _lift monadTrans'ContTOp mOp
    MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap } = monad'ContTOp
  in MonadStateOp {
    _monad'MSOp = monad'ContTOp,
    _modifyAnd = \f -> lift . modifyAnd f,
    _get = lift get,
    _state = lift . state,
    _bind'MS = bind,
    _applicative'MSOp = aOp,
    _pure'MS = pure,
    _ap'MS = ap,
    _functor'MSOp = fOp,
    _fmap'MS = fmap }

--liftCallCC'Reader :: CallCC m -> CallCC (ReaderT r m)
liftCallCC'Reader callCC f = ReaderT $ \ r ->
    callCC $ \ c ->
    runReaderT (f (ReaderT . const . c)) r

{- liftCallCC'State'
  :: ((((a,s) -> m (b,s)) -> m (a,s)) -> m (a,s))
    -> ((a -> (StateT s m) b) -> (StateT s m) a)
    -> (StateT s m) a -}
--liftCallCC'State' :: CallCC m -> CallCC (StateT s m)
liftCallCC'State' callCC f = StateT $ \ s ->
    callCC $ \ c ->
    runStateT (f (\ a -> StateT $ \ s' -> c (a, s'))) s
