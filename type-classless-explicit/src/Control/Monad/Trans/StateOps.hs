{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.Trans.StateOps where

import Prelude hiding (fmap, return, (>>=), pure)
import Control.MonadOps
import Control.Monad.Misc
import Control.Monad.State.Strict (StateT(StateT, runStateT))
import Data.FixOp
import Data.Functor.Identity
import Control.Monad.Trans.ReaderOps
import Control.Monad.Reader (ReaderT)

-- StateT monad transformer

evalStateT :: FunctorOps m -> StateT s m a -> s -> m a
evalStateT functorOps = (_fmap functorOps fst .) . runStateT
--     m s = fst <$> runStateT m s

evalLazyStateT :: FunctorOps m -> StateT s m a -> s -> m a
evalLazyStateT functorOps = let fst' ~(a, _) = a in (_fmap functorOps fst' .) . runStateT

execStateT :: FunctorOps m -> StateT s m a -> s -> m s
execStateT functorOps = (_fmap functorOps snd .) . runStateT
--     m s = snd <$> runStateT m s

execLazyStateT :: FunctorOps m -> StateT s m a -> s -> m s
execLazyStateT functorOps = let snd' ~(_, s) = s in (_fmap functorOps snd' .) . runStateT

-- stub for the three different StateT monad definitions. pure is the same for all three, but bind is different
pkgStateTMonadOps :: ApplicativeOps m -> Bind (StateT s m) -> MonadOps (StateT s m)
pkgStateTMonadOps applicativeOps =
  pkgMonadOps $
    -- :: Pure (StateT s m)
    StateT . ((_pure applicativeOps .) . (, ))

-- Strict monadic operations for StateT
{- instance (Monad m) => Monad (StateT s m) where
       return a = state $ \s -> (a, s)
       m >>= k  = StateT $ \s -> do
           (a, s') <- runStateT m s
           runStateT (k a) s' -}
pkgStrictStateTMonadOps :: MonadOps m -> MonadOps (StateT s m)
pkgStrictStateTMonadOps monadOps =
  pkgStateTMonadOps (_monadApplicativeOps monadOps) $ \ m k ->
    StateT $ \ s -> _bind monadOps (runStateT m s) $ \ (a, s') -> runStateT (k a) s'

-- Lazy monadic operations for StateT
{- instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s' -}
pkgLazyStateTMonadOps :: MonadOps m -> MonadOps (StateT s m)
pkgLazyStateTMonadOps monadOps =
  pkgStateTMonadOps (_monadApplicativeOps monadOps) $ \ m k ->
    StateT $ \ s -> _bind monadOps (runStateT m s) $ \ ~(a, s') -> runStateT (k a) s'

-- Reverse monadic operations for StateT
pkgRevStateTMonadOps :: MonadOps m -> MfixOp m -> (MonadOps (StateT s1 m), MfixOp (StateT s2 m))
pkgRevStateTMonadOps monadOps mfixOp =
  let applicativeOps = _monadApplicativeOps monadOps in
  let functorOps = _applicativeFunctorOps applicativeOps in
  (
{- instance MonadFix m => Monad (StateT s m) where
    m >>= f = StateT $ \s -> do
      rec
        (x, s'') <- runStateT m s'
        (x', s') <- runStateT (f x) s
      return (x', s'') -}
    pkgStateTMonadOps applicativeOps $
    \ m f ->
      StateT $ \ s ->
        _fmap functorOps snd (_mfix mfixOp (\ ~(s', _) ->
          _bind monadOps (runStateT m s') $ \ (x, s'') ->
          _bind monadOps (runStateT (f x) s) $ \ (x', s0') ->
          _pure applicativeOps (s0', (x', s''))
        )),
{- instance MonadFix m => MonadFix (StateT s m) where
    mfix f = StateT $ \s ->
      mfix (\ ~(x, _) -> runStateT (f x) s) -}
    MfixOp $ \ f -> StateT $ \s ->
      _mfix mfixOp (\ ~(x, _) -> runStateT (f x) s))

stateMonadOps :: MonadOps (StateT s Identity)
stateMonadOps = pkgStrictStateTMonadOps identityMonadOps

lazyStateMonadOps :: MonadOps (StateT s Identity)
lazyStateMonadOps = pkgLazyStateTMonadOps identityMonadOps

revStateMonadFixOps :: (MonadOps (StateT s1 Identity), MfixOp (StateT s2 Identity))
revStateMonadFixOps = pkgRevStateTMonadOps identityMonadOps identityFixLiftOp

{- instance MonadTrans StateT where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s) -}
stateTLiftOp :: LiftOp (StateT s)
stateTLiftOp = LiftOp $ \ monadOps m -> StateT $ \ s ->
  _bind monadOps m $ \ a -> _pure (_monadApplicativeOps monadOps) (a, s)

{- class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a -}
type StateOp s m = forall a . (s -> (a, s)) -> m a

data MonadStateOps s m = MonadStateOps
  { _state            :: StateOp s m
  , _monadStateMonadOps :: MonadOps m
  }

-- monadStateMonadOps :: (?monadStateOps :: MonadStateOps s m) => MonadOps m
-- monadStateMonadOps = _monadStateMonadOps ?monadStateOps

-- state :: (?monadStateOps :: MonadStateOps s m) => StateOp s m
-- state = _state ?monadStateOps

get :: MonadStateOps s m -> m s
get monadStateOps = _state monadStateOps $ \ s -> (s, s)

modifyAnd :: MonadStateOps s m -> (s -> s) -> (s -> a) -> m a
modifyAnd monadStateOps f andF = _state monadStateOps $ \s -> let s' = f s in (andF s', s')

putAnd :: MonadStateOps s m -> s -> a -> m a
putAnd monadStateOps s a = modifyAnd monadStateOps (const s) (const a)

modify :: MonadStateOps s m -> (s -> s) -> m ()
modify monadStateOps f = modifyAnd monadStateOps f $ const ()

put :: MonadStateOps s m -> s -> m ()
put monadStateOps s = putAnd monadStateOps s ()

pkgStateTMonadStateOps :: ApplicativeOps m -> MonadOps (StateT s m) -> MonadStateOps s (StateT s m)
pkgStateTMonadStateOps applicativeOps = MonadStateOps (StateT . (_pure applicativeOps .))

{- instance (Monad m) => MonadState s (StateT s m) where
    state = state -- from Control.Monad.Trans.State -}
strictStateTMonadStateOps :: MonadOps m -> MonadStateOps s (StateT s m)
strictStateTMonadStateOps monadOps = pkgStateTMonadStateOps (_monadApplicativeOps monadOps) $ pkgStrictStateTMonadOps monadOps

lazyStateTMonadStateOps :: MonadOps m -> MonadStateOps s (StateT s m)
lazyStateTMonadStateOps monadOps = pkgStateTMonadStateOps (_monadApplicativeOps monadOps) $ pkgLazyStateTMonadOps monadOps

revStateTMonadStateOps :: MonadOps m -> MfixOp m -> (MonadStateOps s1 (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadStateOps monadOps mfixOp =
  let (monadOps', mfixOp') = pkgRevStateTMonadOps monadOps mfixOp in (pkgStateTMonadStateOps (_monadApplicativeOps monadOps) monadOps', mfixOp')

{- instance (MonadState s m) => MonadState s (ReaderT r m) where
    state = lift . state -}
readerTMonadStateOps :: MonadStateOps s m -> MonadStateOps s (ReaderT r m)
readerTMonadStateOps monadStateOps =
  let monadOps = _monadStateMonadOps monadStateOps in
  MonadStateOps (_lift readerTLiftOp monadOps . _state monadStateOps) (readerTMonadOps monadOps)

-- {-
stateTMonadReaderOps :: MonadReaderOps a m -> (MonadOps m -> MonadOps (StateT s m)) -> MonadReaderOps a (StateT s m)
stateTMonadReaderOps monadReaderOps packer = let
    monadOps = _monadReaderMonadOps monadReaderOps
    lift = _lift stateTLiftOp monadOps
  in
  MonadReaderOps
    (lift $ _ask monadReaderOps)
    (\f m -> StateT $ _local monadReaderOps f . runStateT m)
    (lift . _reader monadReaderOps) $
    packer monadOps
--}

{- instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask = lift ask
    local = \f m -> StateT $ local f . runStateT m
    reader = lift . reader -}
strictStateTMonadReaderOps :: MonadReaderOps r m -> MonadReaderOps r (StateT s m)
strictStateTMonadReaderOps = flip stateTMonadReaderOps pkgStrictStateTMonadOps

-- lazyStateTMonadReaderOps :: (?monadReaderOps::MonadReaderOps r m) => MonadReaderOps r (StateT s m)
lazyStateTMonadReaderOps :: MonadReaderOps a m -> MonadReaderOps a (StateT s m)
lazyStateTMonadReaderOps = flip stateTMonadReaderOps pkgLazyStateTMonadOps

revStateTMonadReaderOps :: MonadReaderOps a m -> MfixOp m -> (MonadReaderOps a (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadReaderOps monadReaderOps mfixOp = let
    monadOps = _monadReaderMonadOps monadReaderOps
    lift = _lift stateTLiftOp monadOps
  in
  let (monadOps', mfixOp') = pkgRevStateTMonadOps monadOps mfixOp in
  (
    MonadReaderOps
      (lift $ _ask monadReaderOps)
      (\f m -> StateT $ _local monadReaderOps f . runStateT m)
      (lift . _reader monadReaderOps)
      monadOps',
    mfixOp')
