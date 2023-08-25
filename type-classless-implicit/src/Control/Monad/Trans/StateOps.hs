{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

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

evalStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m a
evalStateT = (fmap fst .) . runStateT
--     m s = fst <$> runStateT m s

evalLazyStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m a
evalLazyStateT = let fst' ~(a, _) = a in (fmap fst' .) . runStateT

execStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m s
execStateT = (fmap snd .) . runStateT
--     m s = snd <$> runStateT m s

execLazyStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m s
execLazyStateT = let snd' ~(_, s) = s in (fmap snd' .) . runStateT

-- stub for the three different StateT monad definitions. pure is the same for all three, but bind is different
pkgStateTMonadOps :: (?applicativeOps :: ApplicativeOps m) => Bind (StateT s m) -> MonadOps (StateT s m)
pkgStateTMonadOps =
  pkgMonadOps $
    -- :: Pure (StateT s m)
    StateT . ((pure .) . (, ))

-- Strict
{- instance (Monad m) => Monad (StateT s m) where
       return a = state $ \s -> (a, s)
       m >>= k  = StateT $ \s -> do
           (a, s') <- runStateT m s
           runStateT (k a) s' -}
pkgStrictStateTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (StateT s m)
pkgStrictStateTMonadOps =
  let ?applicativeOps = monadApplicativeOps in
  pkgStateTMonadOps $ \ m k ->
    StateT $ runStateT m >=> \ (a, s') -> runStateT (k a) s'

-- Lazy
{- instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s' -}
pkgLazyStateTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (StateT s m)
pkgLazyStateTMonadOps =
  let ?applicativeOps = monadApplicativeOps in
  pkgStateTMonadOps $ \m k ->
    StateT $ runStateT m >=> \ ~(a, s') -> runStateT (k a) s'

-- Reverse
pkgRevStateTMonadOps :: (?monadOps :: MonadOps m, ?mfixOp :: MfixOp m) => (MonadOps (StateT s1 m), MfixOp (StateT s2 m))
pkgRevStateTMonadOps =
  let ?applicativeOps = monadApplicativeOps in
  let ?functorOps = applicativeFunctorOps in
  (
{- instance MonadFix m => Monad (StateT s m) where
    m >>= f = StateT $ \s -> do
      rec
        (x, s'') <- runStateT m s'
        (x', s') <- runStateT (f x) s
      return (x', s'') -}
    pkgStateTMonadOps $
    \ m f ->
      StateT $ \ s ->
        mfix (\ ~(s', _) ->
          runStateT m s' >>= \ (x, s'') ->
          runStateT (f x) s >>= \ (x', s0') ->
          pure (s0', (x', s''))
        ) <&> snd,
{- instance MonadFix m => MonadFix (StateT s m) where
    mfix f = StateT $ \s ->
      mfix (\ ~(x, _) -> runStateT (f x) s) -}
    MfixOp $ \ f -> StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s))

stateMonadOps :: MonadOps (StateT s Identity)
stateMonadOps = let ?monadOps = identityMonadOps in pkgStrictStateTMonadOps

lazyStateMonadOps :: MonadOps (StateT s Identity)
lazyStateMonadOps = let ?monadOps = identityMonadOps in pkgLazyStateTMonadOps

revStateMonadFixOps :: (MonadOps (StateT s1 Identity), MfixOp (StateT s2 Identity))
revStateMonadFixOps =
  let ?monadOps = identityMonadOps
      ?mfixOp = identityFixLiftOp
  in pkgRevStateTMonadOps

{- instance MonadTrans StateT where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s) -}
stateTLiftOp :: LiftOp (StateT s)
stateTLiftOp = LiftOp $ \ m -> StateT $ \ s ->
  m >>= \ a -> return (a, s)

{- class Monad m => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()
    state :: (s -> (a, s)) -> m a -}
type StateOp s m = forall a . (s -> (a, s)) -> m a

data MonadStateOps s m = MonadStateOps
  { _state            :: StateOp s m
  , _monadStateMonadOps :: MonadOps m
  }

monadStateMonadOps :: (?monadStateOps :: MonadStateOps s m) => MonadOps m
monadStateMonadOps = _monadStateMonadOps ?monadStateOps

state :: (?monadStateOps :: MonadStateOps s m) => StateOp s m
state = _state ?monadStateOps

get :: (?monadStateOps::MonadStateOps s m) => m s
get = state $ \ s -> (s, s)

modifyAnd :: (?monadStateOps::MonadStateOps s m) => (s -> s) -> (s -> a) -> m a
modifyAnd f andF = state $ \s -> let s' = f s in (andF s', s')

putAnd :: (?monadStateOps::MonadStateOps s m) => s -> a -> m a
putAnd s a = modifyAnd (const s) (const a)

modify :: (?monadStateOps::MonadStateOps s m) => (s -> s) -> m ()
modify f = modifyAnd f $ const ()

put :: (?monadStateOps::MonadStateOps s m) => s -> m ()
put s = putAnd s ()

pkgStateTMonadStateOps :: (?applicativeOps::ApplicativeOps m) => MonadOps (StateT s m) -> MonadStateOps s (StateT s m)
pkgStateTMonadStateOps = MonadStateOps (StateT . (pure .))

{- instance (Monad m) => MonadState s (StateT s m) where
    state = state -- from Control.Monad.Trans.State -}
strictStateTMonadStateOps :: (?monadOps :: MonadOps m) => MonadStateOps s (StateT s m)
strictStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in pkgStateTMonadStateOps pkgStrictStateTMonadOps

lazyStateTMonadStateOps :: (?monadOps :: MonadOps m) => MonadStateOps s (StateT s m)
lazyStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in pkgStateTMonadStateOps pkgLazyStateTMonadOps

revStateTMonadStateOps :: (?monadOps :: MonadOps m, ?mfixOp :: MfixOp m) => (MonadStateOps s1 (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in
  let (monadOps, mfixOp) = pkgRevStateTMonadOps in (pkgStateTMonadStateOps monadOps, mfixOp)

{- instance (MonadState s m) => MonadState s (ReaderT r m) where
    state = lift . state -}
readerTMonadStateOps :: (?monadStateOps::MonadStateOps s m) => MonadStateOps s (ReaderT r m)
readerTMonadStateOps =
  let ?monadOps = monadStateMonadOps
      ?liftOp = readerTLiftOp
  in
  MonadStateOps (lift . state) readerTMonadOps

{- instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask = lift ask
    local = \f m -> StateT $ local f . runStateT m
    reader = lift . reader -}
strictStateTMonadReaderOps :: (?monadReaderOps::MonadReaderOps r m) => MonadReaderOps r (StateT s m)
strictStateTMonadReaderOps = let
    ?monadOps = monadReaderMonadOps
    ?liftOp = stateTLiftOp
  in
  MonadReaderOps
    (lift ask)
    (\f m -> StateT $ local f . runStateT m)
    (lift . reader)
    pkgStrictStateTMonadOps

lazyStateTMonadReaderOps :: (?monadReaderOps::MonadReaderOps r m) => MonadReaderOps r (StateT s m)
lazyStateTMonadReaderOps = let
    ?monadOps = monadReaderMonadOps
    ?liftOp = stateTLiftOp
  in
  MonadReaderOps
    (lift ask)
    (\f m -> StateT $ local f . runStateT m)
    (lift . reader)
    pkgLazyStateTMonadOps

revStateTMonadReaderOps :: (?monadReaderOps::MonadReaderOps r m, ?mfixOp::MfixOp m) => (MonadReaderOps r (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadReaderOps = let
    ?monadOps = monadReaderMonadOps
    ?liftOp = stateTLiftOp
  in
  let (monadOps, mfixOp) = pkgRevStateTMonadOps in
  (
    MonadReaderOps
      (lift ask)
      (\f m -> StateT $ local f . runStateT m)
      (lift . reader)
      monadOps,
    mfixOp)
