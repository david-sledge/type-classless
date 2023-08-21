{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Control.Monad.Trans.StateOps where

import Prelude hiding (fmap, return, (>>=), pure)
import Control.MonadOps
import Control.Monad.Misc
import Control.Monad.State.Strict (StateT(StateT, runStateT))
import Data.FixOp
import Data.Functor.Identity

type StateOp s m = forall a . (s -> (a, s)) -> m a

{- class Monad m => MonadState s m | m -> s where
       get :: m s
       put :: s -> m ()
       state :: (s -> (a, s)) -> m a -}
data MonadStateOps s m = MonadStateOps
  { _state            :: StateOp s m
  , _monadStateSubOps :: (MonadOps m, ApplicativeOps m, FunctorOps m)
  }

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

pkgStateTMonadOps :: (?monadOps :: MonadOps m) => Bind (StateT s m) -> MonadOps (StateT s m)
pkgStateTMonadOps bindF =
  let ?applicativeOps = monadApplicativeOps in
  pkgMonadOps
    -- :: Pure (StateT s m)
    (\ a -> StateT $ \ s -> pure (a, s))
    bindF

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
pkgRevStateTMonadOps :: (?monadOps :: MonadOps m, ?mfixOp :: MfixOp m) => (MonadOps (StateT b1 m), MfixOp (StateT b2 m))
pkgRevStateTMonadOps =
  let ?applicativeOps = monadApplicativeOps in
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
          runStateT m s' >>= \ (x, s0'') ->
          runStateT (f x) s >>= \ (x', s0') ->
          pure (s0', (x', s0''))
        ) >>= (\ (_, r) -> pure r),
{- instance MonadFix m => MonadFix (StateT s m) where
    mfix f = StateT $ \s ->
      mfix (\ ~(x, _) -> runStateT (f x) s) -}
    MfixOp $ \ f -> StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s))

stateMonadOps :: MonadOps (StateT s Identity)
stateMonadOps = let ?monadOps = identityMonadOps in pkgStrictStateTMonadOps

lazyStateMonadOps :: MonadOps (StateT b1 Identity)
lazyStateMonadOps = let ?monadOps = identityMonadOps in pkgLazyStateTMonadOps

revStateMonadFixOps :: (MonadOps (StateT b1 Identity), MfixOp (StateT b2 Identity))
revStateMonadFixOps =
  let ?monadOps = identityMonadOps
      ?mfixOp = identityFixLiftOp
  in pkgRevStateTMonadOps

{- instance MonadTrans StateT where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s) -}
stateTLift :: LiftOp (StateT s)
stateTLift = LiftOp $ \ m -> StateT $ \ s ->
  m >>= \ a -> return (a, s)

pkgMonadStateOps :: (?applicativeOps::ApplicativeOps m) => (MonadOps (StateT s m), ApplicativeOps (StateT s m), FunctorOps (StateT s m)) -> MonadStateOps s (StateT s m)
pkgMonadStateOps = MonadStateOps (StateT . (pure .))

{- instance (Monad m) => MonadState s (StateT s m) where
    state = state -- from Control.Monad.Trans.State -}
strictStateTMonadStateOps :: (?monadOps :: MonadOps m) => MonadStateOps s (StateT s m)
strictStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in pkgMonadStateOps (
    let stateTApplicativeOps = _monadApplicativeOps pkgStrictStateTMonadOps
    in (pkgStrictStateTMonadOps, stateTApplicativeOps, _applicativeFunctorOps stateTApplicativeOps))

lazyStateTMonadStateOps :: (?monadOps :: MonadOps m) => MonadStateOps s (StateT s m)
lazyStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in pkgMonadStateOps (
    let stateTApplicativeOps = _monadApplicativeOps pkgLazyStateTMonadOps
    in (pkgLazyStateTMonadOps, stateTApplicativeOps, _applicativeFunctorOps stateTApplicativeOps))

revStateTMonadStateOps :: (?monadOps :: MonadOps m, ?mfixOp :: MfixOp m) => (MonadStateOps s1 (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadStateOps =
  let ?applicativeOps = monadApplicativeOps in
  let (monadOps, mfixOp) = pkgRevStateTMonadOps in (pkgMonadStateOps (
    let stateTApplicativeOps = _monadApplicativeOps monadOps
    in (monadOps, stateTApplicativeOps, _applicativeFunctorOps stateTApplicativeOps)), mfixOp)

{- instance (MonadReader r m) => MonadReader r (StateT s m) where
       ask = lift ask
       local = \f m -> StateT $ local f . runStateT m
       reader = lift . reader -}
--monadReader'StateTOp ::
--    MonadReaderOp r m -> MonadReaderOp r (StateT s m)
-- stateTMonadReaderOps mrOps = let
--     mOps = _monadReaderMonadOps mrOps
--     lift = stateTLift mOps
--   in
--   MonadReaderOps (lift $ _ask mrOps)
--     (\f m -> StateT $ _local mrOps f . runStateT m)
--     (lift . _reader mrOps)
--     $ pkgStrictStateTMonadOps mOps

{- instance (MonadState s m) => MonadState s (ReaderT r m) where
       get   = lift get
       put   = lift . put
       state = lift . state -}
--monadState'ReaderTOp :: MonadStateOp s m -> MonadStateOp s (ReaderT r m)
-- readerTMonadStateOps msOps = let
--     mOps = _monadStateMonadOps msOps
--     lift = _lift readerTMonadTransOp mOps
--   in
--   MonadStateOps
--     (\f -> lift . _modifyAnd msOps f)
--     (lift $ _get msOps)
--     (lift . _state msOps)
--     $ readerTMonadOps mOps
--}
