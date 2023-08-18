{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Control.Monad.Trans.StateOps where

import Prelude hiding (fmap, return, (>>=), pure)
import Control.MonadOps
import Control.Monad.Misc
import Control.Monad.Trans.ReaderOps
import Control.Monad.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Data.FixOp
import Data.Functor.Identity

type StateOp s m = forall a . (s -> (a, s)) -> m a

{- class Monad m => MonadState s m | m -> s where
       get :: m s
       put :: s -> m ()
       state :: (s -> (a, s)) -> m a -}
data MonadStateOps s m = MonadStateOps
  { _state              :: StateOp s m
  , _monadStateMonadOps :: MonadOps m
  }

monadStateMonadOps :: (?monadStateOps :: MonadStateOps r m) => MonadOps m
monadStateMonadOps = _monadStateMonadOps ?monadStateOps

monadStateApplicativeOps :: (?monadStateOps :: MonadStateOps r m) => ApplicativeOps m
monadStateApplicativeOps = _applicativeOps monadStateMonadOps

monadStatePure :: (?monadStateOps :: MonadStateOps r m) => Pure m
monadStatePure = _pure monadStateApplicativeOps

monadStateAp :: (?monadStateOps :: MonadStateOps r m) => Ap m
monadStateAp = _ap monadStateApplicativeOps

monadStateFmap :: (?monadStateOps :: MonadStateOps r m) => Fmap m
monadStateFmap =
  let ?applicativeOps = monadStateApplicativeOps in applicativeFmap

{- class (MonadFix m) => MonadRevState s m | m -> s where
     get :: m s
     get = state $ \s -> (s, s)

     put :: s -> m ()
     put s = state $ \_ -> ((), s)

     state :: (s -> (a, s)) -> m a
     state f = do
       rec
         let ~(a, s') = f s
         put s'
         s <- get
       return a -}

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

{- evalStateT :: (Monad m) => StateT s m a -> s -> m a -}
-- evalStateT :: (?monadOps::MonadOps m) => StateT s m a -> s -> m a
-- evalStateT m s = runStateT m s >>= return . fst
{- evalStateT :: (Monad m) => StateT s m a -> s -> m a -}
evalStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m a
evalStateT = (fmap fst .) . runStateT
{-
evalStateT m s = fst <$> runStateT m s
--}

-- evalLazyStateT :: (?monadOps::MonadOps m) => StateT s m a -> s -> m a
-- evalLazyStateT m s = runStateT m s >>= \ ~(a, _) -> return a
evalLazyStateT :: (?functorOps::FunctorOps m) => StateT s m a -> s -> m a
evalLazyStateT = let fst' ~(a, _) = a in (fmap fst' .) . runStateT

{- execStateT :: (Monad m) => StateT s m a -> s -> m s -}
execStateT :: (?monadOps::MonadOps m) => StateT s m a -> s -> m s
execStateT m s = runStateT m s >>= return . snd

execLazyStateT :: (?monadOps::MonadOps m) => StateT s m a -> s -> m s
execLazyStateT m s = runStateT m s >>= \ ~(_, s') -> return s'

stateT :: (?monadOps :: MonadOps m) => (s -> (a, s)) -> StateT s m a
stateT = StateT . (return .)

{- instance (Monad m) => Monad (StateT s m) where
       return a = state $ \s -> (a, s)
       m >>= k  = StateT $ \s -> do
           (a, s') <- runStateT m s
           runStateT (k a) s' -}
stateTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (StateT s m)
stateTMonadOps =
  monadOps (stateT . (, )) $ \m k ->
    StateT $ \s -> runStateT m s >>= \(a, s') -> runStateT (k a) s'

lazyStateTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (StateT s m)
lazyStateTMonadOps =
  monadOps (stateT . (, )) $ \m k ->
    StateT $ \s ->
      runStateT m s >>= \ ~(a, s') -> runStateT (k a) s'

stateMonadOps :: MonadOps (StateT s Identity)
stateMonadOps = let ?monadOps = identityMonadOps in stateTMonadOps

{- instance MonadFix m => MonadFix (StateT s m) where
     mfix f = StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s) -}
revStateTMonadOps :: (?monadOps :: MonadOps m, ?mfix :: MfixOp m) => (MonadOps (StateT b1 m), MfixOp (StateT b2 m))
revStateTMonadOps =
  let MfixOp mfix = ?mfix
  in
  ( monadOps
    -- pure
    (stateT . (, ))
    -- bind
    (\m f ->
    StateT $ \s ->
    mfix (\ ~(s', _) ->
      runStateT m s' >>= \(x, s0'') ->
      runStateT (f x) s >>= \(x', s0') ->
      return (s0', (x', s0''))
    ) >>= (\(_, r) -> return r))
  , MfixOp $ \ f -> StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s)
  )

revStateMonadFixOps :: (MonadOps (StateT b1 Identity), MfixOp (StateT b2 Identity))
revStateMonadFixOps =
  let ?monadOps = identityMonadOps
      ?mfix = identityFixLiftOp
  in revStateTMonadOps

{- instance MonadTrans StateT where
       lift m = StateT $ \s -> do
           a <- m
           return (a, s) -}
stateTLift :: Lift (StateT s)
stateTLift m = StateT $ \ s ->
  m >>= \ a -> return (a, s)

{- instance (Monad m) => MonadState s (StateT s m) where
       state = state -- from Control.Monad.Trans.State -}
stateTMonadStateOps :: (?monadOps :: MonadOps m) => MonadStateOps s (StateT s m)
stateTMonadStateOps = MonadStateOps stateT stateTMonadOps

revStateTMonadStateOps :: (?monadOps :: MonadOps m, ?mfix :: MfixOp m) => (MonadStateOps s1 (StateT s1 m), MfixOp (StateT s2 m))
revStateTMonadStateOps =
  let (monadOps', mfixOp) = revStateTMonadOps
  in
  ( MonadStateOps stateT monadOps'
  , mfixOp
  )

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
--     $ stateTMonadOps mOps

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
