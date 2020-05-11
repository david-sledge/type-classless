{-# LANGUAGE Rank2Types #-}

module Control.Monad.Trans.StateOp where

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.Trans.ClassOp
import Control.Monad.Trans.ReaderOp
import Control.Monad.Trans.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Data.Functor.Identity

type ModifyAnd s m = forall a . (s -> s) -> (s -> a) -> m a

type StateF s m = forall a . (s -> (a, s)) -> m a

{- class Monad m => MonadState s m | m -> s where
       get :: m s
       put :: s -> m ()
       state :: (s -> (a, s)) -> m a -}
data MonadStateOp s m = MonadStateOp {
    _modifyAnd        :: ModifyAnd s m,
    _get              :: m s,
    _state            :: StateF s m,
    _monad'MSOp       :: MonadOp m,
    _bind'MS          :: Bind m,
    _applicative'MSOp :: ApplicativeOp m,
    _pure'MS          :: Pure m,
    _ap'MS            :: Ap m,
    _functor'MSOp     :: FunctorOp m,
    _fmap'MS          :: Fmap m }

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

type MonadRevStateOp s m = (MonadStateOp s m, MonadFixOp m)

--_putAnd :: MonadStateOp s m -> s -> a -> m a
_putAnd msOp s a = _modifyAnd msOp (const s) (const a)

--_put :: MonadStateOp s m -> s -> m ()
_put msOp s = _putAnd msOp s ()

--_modify :: MonadStateOp s m -> (s -> s) -> m ()
_modify msOp f = _modifyAnd msOp f (\_ -> ())

{- get :: (Monad m) => StateT s m s
   get = StateT $ \s -> return (s, s) -}
--get :: MonadOp m -> StateT s m s
get mOp = StateT $ \s -> _pure'M mOp (s, s)

--modifyAnd :: MonadOp m -> ModifyAnd s (StateT s m)
modifyAnd mOp f andf = StateT $ \s -> let s' = f s in _pure'M mOp (andf s', s')

--putAnd :: MonadOp m -> s -> a -> StateT s m a
putAnd mOp s and = StateT $ \_ -> _pure'M mOp (and, s)

{- put :: (Monad m) => s -> StateT s m ()
   put s = StateT $ \_ -> return ((), s) -}
--put :: MonadOp m -> s -> StateT s m ()
put mOp s = putAnd mOp s ()

--modify :: MonadOp m -> (s -> s) -> StateT s m ()
modify mOp f = modifyAnd mOp f (\_ -> ())

{- state :: (Monad m) => (s -> (a, s)) -> StateT s m a
   state f = StateT (return . f) -}
--state :: MonadOp m -> StateF s (StateT s m)
state mOp f = StateT (_pure'M mOp . f)

{- evalStateT :: (Monad m) => StateT s m a -> s -> m a -}
--evalStateT :: MonadOp m -> StateT s m b -> s -> m b
evalStateT (MonadOp { _pure'M = pure, _bind = (>>=)}) m s =
  runStateT m s >>= (pure . fst)

evalLazyStateT (MonadOp { _pure'M = pure, _bind = (>>=)}) m s =
  runStateT m s >>= \ ~(a, _) ->
  pure a

{- execStateT :: (Monad m) => StateT s m a -> s -> m s -}
--execStateT :: MonadOp m -> StateT s m b -> s -> m s
execStateT (MonadOp { _pure'M = pure, _bind = (>>=)}) m s =
  runStateT m s >>= (pure . snd)

execLazyStateT (MonadOp { _pure'M = pure, _bind = (>>=)}) m s =
  runStateT m s >>= \ ~(_, s) ->
  pure s

{- instance (Monad m) => Monad (StateT s m) where
       return a = state $ \s -> (a, s)
       m >>= k  = StateT $ \s -> do
           (a, s') <- runStateT m s
           runStateT (k a) s' -}
--monad'StateTOp :: MonadOp m -> MonadOp (StateT s m)
monad'StateTOp mOp@(MonadOp { _bind = (>>=)}) =
  monadOp (\a -> state mOp $ \s -> (a, s)) $ \m k ->
    StateT $ \s ->
      runStateT m s >>= \(a, s') ->
      runStateT (k a) s'

monad'LazyStateTOp mOp@(MonadOp { _bind = (>>=)}) =
  monadOp (\a -> state mOp $ \s -> (a, s)) $ \m k ->
    StateT $ \s ->
      runStateT m s >>= \ ~(a, s') ->
      runStateT (k a) s'

--monad'StateOp :: MonadOp (StateT s Identity)
monad'StateOp = monad'StateTOp monad'IdentityOp

{- instance MonadFix m => MonadFix (StateT s m) where
     mfix f = StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s) -}

--monadFix'RevStateTOp :: MonadFixOp m -> MonadFixOp (StateT b m)
monadFix'RevStateTOp
  (MonadFixOp {
      _monad'MFOp = mOp,
      _mfix = mfix,
      _bind'MF = (>>=),
      _pure'MF = pure }) =
  monadFixOp
    (\a -> state mOp $ \s -> (a, s))
    (\m f ->
    StateT $ \s ->
    mfix (\ ~(s', _) ->
      runStateT m s' >>= \(x, s0'') ->
      runStateT (f x) s >>= \(x', s0') ->
      pure (s0', (x', s0''))
    ) >>= (\(_, r) -> pure r)) $
    \f -> StateT $ \s ->
       mfix (\ ~(x, _) -> runStateT (f x) s)

--monadFix'RevStateOp :: MonadFixOp (StateT b Identity)
monadFix'RevStateOp = monadFix'RevStateTOp monadFix'IdentityOp

{- instance MonadTrans StateT where
       lift m = StateT $ \s -> do
           a <- m
           return (a, s) -}
--monadTrans'StateTOp :: MonadTransOp (StateT s)
monadTrans'StateTOp = MonadTransOp $
  \(MonadOp { _pure'M = pure, _bind = (>>=)}) m ->
    StateT $ \s -> m >>= \a -> pure (a, s)

{- instance (MonadReader r m) => MonadReader r (StateT s m) where
       ask = lift ask
       local = \f m -> StateT $ local f . runStateT m
       reader = lift . reader -}
--monadReader'StateTOp ::
--    MonadReaderOp r m -> MonadReaderOp r (StateT s m)
monadReader'StateTOp (MonadReaderOp {
    _monad'MROp = mOp,
    _ask = ask,
    _local = local,
    _reader = reader}) = let
    lift = _lift monadTrans'StateTOp mOp
    monadOp@(MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap
      }) = monad'StateTOp mOp
  in
  MonadReaderOp {
    _monad'MROp = monadOp,
    _ask = lift ask,
    _local = \f m -> StateT $ local f . runStateT m,
    _reader = lift . reader,
    _bind'MR = bind,
    _applicative'MROp = aOp,
    _pure'MR = pure,
    _ap'MR = ap,
    _functor'MROp = fOp,
    _fmap'MR = fmap }

{- instance (Monad m) => MonadState s (StateT s m) where
       get = get -- from Control.Monad.Trans.State
       put = put
       state = state -}
--monadState'StateTOp :: MonadOp m -> MonadStateOp s (StateT s m)
monadState'StateTOp mOp = let
    monadOp@(MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap }) = monad'StateTOp mOp
  in
  MonadStateOp {
    _monad'MSOp = monadOp,
    _modifyAnd = modifyAnd mOp,
    _get = get mOp,
    _state = state mOp,
    _bind'MS = bind,
    _applicative'MSOp = aOp,
    _pure'MS = pure,
    _ap'MS = ap,
    _functor'MSOp = fOp,
    _fmap'MS = fmap }

{- instance MonadFix m => MonadRevState s (StateT s m) where
     get = get
     put = put
     state = state -}
--monadRevState'StateTOp :: MonadFixOp m -> MonadRevStateOp s (StateT s m)
monadRevState'StateTOp mfOp@(MonadFixOp {_monad'MFOp = mOp}) = let
    monadFixOp@(MonadFixOp {
        _monad'MFOp = monadOp,
        _bind'MF = bind,
        _applicative'MFOp = aOp,
        _pure'MF = pure,
        _ap'MF = ap,
        _functor'MFOp = fOp,
        _fmap'MF = fmap }) = monadFix'RevStateTOp mfOp
  in
  (
    MonadStateOp {
      _monad'MSOp = monadOp,
      _modifyAnd = modifyAnd mOp,
      _get = get mOp,
      _state = state mOp,
      _bind'MS = bind,
      _applicative'MSOp = aOp,
      _pure'MS = pure,
      _ap'MS = ap,
      _functor'MSOp = fOp,
      _fmap'MS = fmap },
    monadFixOp )

{- instance (MonadState s m) => MonadState s (ReaderT r m) where
       get   = lift get
       put   = lift . put
       state = lift . state -}
--monadState'ReaderTOp :: MonadStateOp s m -> MonadStateOp s (ReaderT r m)
monadState'ReaderTOp (MonadStateOp {
        _monad'MSOp = mOp,
        _modifyAnd = modifyAnd,
        _get = get,
        _state = state
      }) = let
    lift = _lift monadTrans'ReaderTOp mOp
    monadOp@(MonadOp {
        _bind = bind,
        _applicative'MOp = aOp,
        _pure'M = pure,
        _ap'M = ap,
        _functor'MOp = fOp,
        _fmap'M = fmap }) = monad'ReaderTOp mOp
  in
  MonadStateOp {
    _monad'MSOp = monad'ReaderTOp mOp,
    _modifyAnd = \f -> lift . modifyAnd f,
    _get = lift get,
    _state = lift . state,
    _bind'MS = bind,
    _applicative'MSOp = aOp,
    _pure'MS = pure,
    _ap'MS = ap,
    _functor'MSOp = fOp,
    _fmap'MS = fmap }
