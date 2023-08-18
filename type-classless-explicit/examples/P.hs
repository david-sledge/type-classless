{-# LANGUAGE Rank2Types #-}

module Example.P where

import Control.MonadOp
import Control.Monad.Trans.ClassOp
import Control.Monad.Trans.Reader (ReaderT(..), reader, runReader)
import Control.Monad.Trans.ReaderOp
import Data.Function
import Control.Monad.Trans.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Control.Monad.Trans.StateOp
import Control.Monad.Trans.Cont (ContT(..), Cont, cont, runCont)
import Control.Monad.Trans.ContOp

--ex3 :: Num b => Bool -> ContT r Identity b -> ContT r Identity b
ex3 bool h = let
    pure = _pure'M monad'ContTOp
    (>>=) = _bind monad'ContTOp
  in
  pure 1 >>= \a ->
  (cont (\fred -> if bool then h else fred 10)) >>= \b ->
  pure $ a+b

test3True = runCont (ex3 True "escape") show

test3False = runCont (ex3 False "escape") show

--ex4 :: Num b => Bool -> ([Char] -> m r) -> ContT r m b
ex4 h = let
    pure = _pure'M monad'ContTOp
    (>>=) = _bind monad'ContTOp
  in
  pure 1 >>= \a ->
  (ContT (\fred -> h "escape" a fred)) >>= \b ->
  pure $ a+b

test4True = runContT (ex4 (\str n c -> c n)) print

test4False = runContT (ex4 (\str n c -> putStrLn str)) print

ex5 b h = let
    pure = _pure'M monad'ContTOp
    (>>=) = _bind monad'ContTOp
  in
  pure 1 >>= \a ->
  cont (\c -> h b (c 10 ++ c 20)) >>= \b ->
  pure $ a+b

test5 b = runCont (ex5 b (\b' c -> if b' then c else "escape")) show

--------------------------------------------------------------------------------
monadReader'ReaderContOp@(MonadReaderOp { _monad'MROp = monad'ReaderContTOp }) =
  monadReader'ReaderTOp monad'ContTOp

--------------------------------------------------------------------------------

--setjmp :: (a -> b) -> (((a -> a) -> b) -> c) -> c
setjmp pure callCC = callCC $ pure . fix

data Catchers n a = Catchers {
    zero :: (n -> a) -> a,
    negative :: (n -> a) -> n -> a }

{- exceptionalT
  :: (Ord a, Floating a) =>
     a -> TryT r m (Catchers a (m r)) (a, a) -}
exceptionalT n = let
    msOp = monadState'StateTOp monad'ReaderContTOp
    pure = _pure'MS msOp
    (>>=) = _bind'MS msOp
    modifyAnd = _modifyAnd msOp
    get = _get msOp
    (MonadReaderOp {
        _ask = ask }) =
      monadReader'StateTOp monadReader'ReaderContOp
    m >> k = m >>= const k
    callCC' = liftCallCC'State' $ liftCallCC'Reader callCC
  in
  runStateT (
      get >>= \s ->
      if s <= 0
      then
        ask >>= \(Catchers z neg) ->
        _lift monadTrans'StateTOp monad'ReaderContTOp .
          _lift monadTrans'ReaderTOp monad'ContTOp . ContT $ \c ->
            if s == 0
            then z c
            else neg c s
      else
        setjmp pure callCC' >>= \l ->
        get >>= \t ->
        if abs(t - 1) < 0.01
        then pure t
        else modifyAnd sqrt id >> l
    ) n

exceptionalTTest1 n = runContT (
    runReaderT (exceptionalT n)
      (Catchers (\c -> "0") (\c s -> "no negatives! " ++ show s)))
  show

exceptionalTTest2 n = runContT (
    runReaderT (exceptionalT n)
      (Catchers (\c -> "0") (\c s -> "no negatives! " ++ show s)))
  show
