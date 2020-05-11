{-# LANGUAGE Rank2Types #-}

module Control.MonadOp where

type Fmap f = forall a b . (a -> b) -> f a -> f b

{- class Functor f where
    fmap :: (a -> b) -> f a -> f b -}
newtype FunctorOp f = FunctorOp { _fmap :: Fmap f }

type Pure f = forall a . a -> f a
type Ap f = forall a b . f (a -> b) -> f a -> f b

{- class Functor f => Applicative f where
    pure  :: a -> f a
    ap :: f (a -> b) -> f a -> f b -}
data ApplicativeOp f = ApplicativeOp {
    _pure         :: Pure f,
    _ap           :: Ap f,
    _functor'AOp  :: FunctorOp f,
    _fmap'A       :: Fmap f }

applicativeOp :: Pure f -> Ap f -> ApplicativeOp f
applicativeOp pure ap = let
    fmap = ap . pure
  in
  ApplicativeOp pure ap (FunctorOp fmap) fmap

type Bind m = forall a b . m a -> (a -> m b) -> m b

{- class Monad m where
       return :: a -> m a
       (>>=) :: m a -> (a -> m b) -> m b -}
-- Monad "Op"erations
data MonadOp m = MonadOp {
    _bind             :: Bind m,
    _applicative'MOp  :: ApplicativeOp m,
    _pure'M           :: Pure m,
    _ap'M             :: Ap m,
    _functor'MOp      :: FunctorOp m,
    _fmap'M           :: Fmap m }

monadOp :: Pure m -> Bind m -> MonadOp m
monadOp pure bind = let
    (>>=) = bind
    ap = \m1 m2 -> m1 >>= \x1 -> m2 >>= \x2 -> pure (x1 x2)
    fmap = \f x -> x >>= (pure . f)
    functorOp = FunctorOp fmap
  in
  MonadOp {
    _bind = bind,
    _applicative'MOp = ApplicativeOp {
      _pure = pure,
      _ap = ap,
      _functor'AOp = functorOp,
      _fmap'A = fmap },
    _pure'M = pure,
    _ap'M = ap,
    _functor'MOp = functorOp,
    _fmap'M = fmap }
