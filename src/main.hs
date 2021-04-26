{-# LANGUAGE Rank2Types #-}

import Control.MonadOp
import Control.Monad.FixOp
import Control.Monad.Misc
import Control.Monad.Trans.Reader (ReaderT(..), Reader, runReader)
import Control.Monad.Trans.ReaderOp
import Control.Monad.Trans.State.Strict (StateT(..), State, runState, evalState,
    execState)
import Data.Function
import Data.Functor.Identity
import Example.Data.DataPackTest
import Data.TraversableOp
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Control.Monad.Trans.StateOp
import Control.Monad.Trans.Cont (ContT(..), Cont, cont, runCont)
import Control.Monad.Trans.ContOp

-- http://www.haskellforall.com/2012/05/scrap-your-type-classes.html
{- test :: (MonadState r m, MonadReader r m) => m [r]
   test = do
     s <- get
     r <- ask
     put r
     return [r, s] -}
--test :: MonadStateOp r m -> MonadReaderOp r m -> m [r]
test = \msOp mrOp -> let
    (>>=) = _bind'MR mrOp
    ask = _ask mrOp
    get = _get msOp
    putAnd = _putAnd msOp
    -- the following could also be used. They SHOULD return the equivalent thing
    --(>>=) = _bind (_monad'MSOp sOp)
  in
  get >>= \s ->
  ask >>= \r ->
  (putAnd r [r, s])

--example1 :: ReaderT r (StateT r Identity) [r]
example1 = test
    (monadState'ReaderTOp . monadState'StateTOp $ monad'IdentityOp)
    (monadReader'ReaderTOp . monad'StateTOp $ monad'IdentityOp)

--example2 :: StateT a (ReaderT a Identity) [r]
example2 = test
    (monadState'StateTOp . monad'ReaderTOp $ monad'IdentityOp)
    (monadReader'StateTOp . monadReader'ReaderTOp $ monad'IdentityOp)

--run1, run2 :: ([r], Char)
run1 = runIdentity $ runStateT (runReaderT example1 'A') 'B'
run2 = runIdentity $ runReaderT (runStateT example2 'B') 'A'

--------------------------------------------------------------------------------

--http://www.cs.toronto.edu/~trebla/personal/haskell/cont-monad.xhtml
{-
> loop :: (Monad m) => m a -> m b
> loop p = fix (p >>)
--}

--loop :: (a -> (t -> b) -> b) -> a -> b
loop (>>=) p =
  let m >> k = m >>= (const k) in
  fix (p >>)

{-
> fun :: Int -> Int
> fun n = runCont (evalStateT p n) id
>     where
>       p = do
>         { callCC $ \c -> loop $ do
>                               { modify (+ 1)
>                               ; s <- get
>                               ; when (s == 5) (c s)
>                               }
>         }
--}

--(Num s, Ord s) => MonadContOp m -> MonadStateOp s m -> m s
p callCC msOp = let
    pure = _pure'MS msOp
    (>>=) = _bind'MS msOp
    modifyAnd = _modifyAnd msOp
    get = _get msOp
    state = _state msOp
  in
  callCC $ \c -> loop (>>=) $
    modifyAnd (+ 1) id >>= \s ->
    if (s > 4) then (c s) else pure s

--(Num a, Ord a) => a -> a
fun n = runCont (
    runStateT (
      p (liftCallCC'State' callCC) $ monadState'StateTOp monad'ContTOp
    ) n
  ) id

--(Num a, Ord a) => a -> (a, a)
nuf = runState . (
    runContT . p callCC . monadState'ContTOp $
      monadState'StateTOp monad'IdentityOp
  ) . _pure'M $ monad'StateOp

--{-
data Provider s m r o = Provider {
  callCCf :: CallCC (SRC s m r o),
  monadStateOp :: MonadStateOp s (SRC s m r o),
  otherStuff :: o }

type SRC s m r o = StateT s (ReaderT (Provider s m r o) (ContT r m))

--{-
p2 mrOp = let
    pure = _pure'MR mrOp
    (>>=) = _bind'MR mrOp
    ask = _ask mrOp
  in
  ask >>= \(Provider callCC (MonadStateOp {
          _modifyAnd = modifyAnd, _get = get }) o) ->
  callCC $ \c -> loop (>>=) $
    modifyAnd (+ 1) id >>= \s ->
    if (s > 4) then (c s) else pure o

fun2 n = let
    monad'ReaderCont = monad'ReaderTOp monad'ContTOp
    callCC' = liftCallCC'State' $ liftCallCC'Reader callCC
    monadStateOp = monadState'StateTOp monad'ReaderCont
    provider = Provider callCC' monadStateOp ()
  in
  runCont (
    runReaderT (evalStateT monad'ReaderCont (
      p2 . monadReader'StateTOp . monadReader'ReaderTOp $ monad'ContTOp
    ) n) provider
  ) id
--}

--p3 :: (Num s, Ord s) => Feeder s m o -> m s
p3 (mOp, (msOp, callCC)) = let
    pure = _pure'M mOp
    (>>=) = _bind mOp
    modifyAnd = _modifyAnd msOp
    get = _get msOp
  in
  callCC $ \c -> loop (>>=) $
    get >>= \s ->
    if (s > 4) then (c s) else modifyAnd (+ 1) id >>= const (pure ())

--(Num a, Ord a) => a -> a
fun3 n = runCont (
    evalStateT monad'ContTOp (
        p3 $ let
            msOp@(MonadStateOp { _monad'MSOp = mOp, _modifyAnd = modifyAnd,
              _get = get }) =
              monadState'StateTOp monad'ContTOp
          in (
            mOp,
            (msOp,
            liftCallCC'State' callCC)
          )
    ) n
  ) id
--}

--------------------------------------------------------------------------------

-- more from http://www.cs.toronto.edu/~trebla/personal/haskell/cont-monad.xhtml

{-
> setjmp :: (MonadCont m) => m (m b)
> setjmp = callCC (\c -> return (fix c))
-}
setjmp pure callCC = callCC (\c -> pure (fix c))

{-
> type RSC e s y = ReaderT (Thrower e s y) (StateT s (Cont y))
> newtype Thrower e s y = Thrower (e -> RSC e s y ())
-}
type SRC2 m e s y = StateT s (ReaderT (Thrower m e s y) (ContT y m))
newtype Thrower m e s y = Thrower {
    catch :: SRCOp m e s y -> e -> SRC2 m e s y () }

type SRCOp m e s y = (
    MonadOp (SRC2 m e s y), (
      MonadStateOp s (SRC2 m e s y), (
        MonadReaderOp (Thrower m e s y) (SRC2 m e s y),
        CallCCOp (SRC2 m e s y)
      )
    )
  )

{-
> throwI :: e -> RSC e s y b
> throwI e = do
>   { Thrower thrower <- ask
>   ; thrower e
>   ; undefined
>   }
-}
throwI ms@(MonadOp { _bind = (>>=) }, (_, (mrOp, _))) e = let
    ask = _ask mrOp
  in
  ask >>= \(Thrower thrower) ->
  thrower ms e >>= const undefined

{-
> catchI :: RSC e s y a
>        -> (e -> RSC e s y a)
>        -> RSC e s y a
> catchI action handler =
>     callCC (\c -> local (mkt c) action)
>     where mkt c r = Thrower (\e -> with r (handler e >>= c))
>           with r m = local (const r) m
-}
catchI ms@(MonadOp {_bind = (>>=)}, (_, (mrOp, CallCCOp callCC))) action handler =
  let
    local = _local mrOp
  in
    callCC (
        \c -> local (
          \r -> Thrower (\ms' e -> (local $ const r) (handler ms' e >>= c))
        ) action
      )

{-
> data Bad = Zero | Neg deriving Show
-}
data Bad = Zero | Neg deriving Show

{-
> exceptional :: Double -> Double
> exceptional n = runCont (evalStateT (runReaderT p topthrow) n) id
>     where
>       topthrow = Thrower (\e -> error ("unhandled exception " ++ show e))
>       p = do
>         { catchI q handler
>         }
>       q = do
>         { s <- get
>         ; when (s < 0) (throwI Neg)
>         ; when (s <= 0) (throwI Zero)
>         ; l <- setjmp
>         ; t <- get
>         ; if abs(t - 1) < 0.01 then return t else modify sqrt >> l
>         }
>       handler Zero = return 0
>       handler Neg = throwI Neg
-}
--exceptional :: Double -> Double
exceptional n = let
    monad'ReaderContTOp = monad'ReaderTOp monad'ContTOp
    msOp = monadState'StateTOp $ monad'ReaderContTOp
    mrOp = monadReader'StateTOp $ monadReader'ReaderTOp monad'ContTOp
    mcOp = CallCCOp (liftCallCC'State' $ liftCallCC'Reader callCC)
    mOp = _monad'MSOp msOp
  in
  runCont (runReaderT (evalStateT monad'ReaderContTOp (
    p (mOp, (msOp, (mrOp, mcOp)))) n) topthrow) id
  where
    topthrow = Thrower (\ms e -> error ("unhandled exception " ++ show e))
    p ms = catchI ms (q ms) handler
    q ms@(mOp,
      ((MonadStateOp { _modifyAnd = modifyAnd, _get = get }), (_, CallCCOp callCC))) = let
        pure = _pure'M mOp
        (>>=) = _bind mOp
        m >> k = m >>= const k
        modify f = modifyAnd f (const ())
      in
      get >>= \s ->
      (if s < 0 then throwI ms Neg else pure ()) >>
      (if s == 0 then throwI ms Zero else pure ()) >>
      setjmp pure callCC >>= \l ->
      get >>= \t ->
      if abs(t - 1) < 0.01 then pure t else modify sqrt >> l
    handler ms Zero = _pure'M (fst ms) 0
    handler ms Neg = throwI ms Neg

lastOccurrence :: Int -> State [Int] Bool
lastOccurrence x = let
    (msOp, mfOp) = monadRevState'StateTOp monadFix'IdentityOp
    get = _get msOp
    mfix = _mfix mfOp
    mOp = _monad'MFOp mfOp
    (>>=) = _bind'MF mfOp
    pure = _pure'MF mfOp
    m >> k = m >>= const k
    put = _put msOp
  in
  mfix (\ ~(xs') ->
      put (x : xs') >>
      get >>= \xs ->
      pure xs
    ) >>= \xs -> pure . not $ elem x xs

lastOccurrences :: [Int] -> State [Int] [Bool]
lastOccurrences xs = listTraverse
  (_applicative'MFOp monadFix'RevStateOp) lastOccurrence xs

exampleValue :: [Bool]
exampleValue = flip evalState [] $ lastOccurrences [3,4,6,7,4,3,5,7]

expectedResult :: [Bool]
expectedResult = [False,False,True,False,True,True,True,True]

revStateTest = let
    (>>=) = _bind monad'IOOp
    pure = _pure'M monad'IOOp
  in
  (timeout 1000000 $ return $! exampleValue == expectedResult) >>= \b ->
  if b == Just True
    then putStrLn "reverse state pass"
    else putStrLn "reverse state fail" >>= const exitFailure

main = let m1 >> m2 = (_bind monad'IOOp) m1 $ const m2 in
  (print run1) >>
  (print run2) >>
  (print $ fun 1) >>
  (print $ nuf 1.5) >>
  (print $ fun2 1.3) >>
  (print $ fun3 5) >>
  testUnpackT >>
  revStateTest
