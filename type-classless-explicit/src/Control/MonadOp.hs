{-# LANGUAGE Rank2Types #-}

module Control.MonadOp where

{- class Functor f where
    fmap :: (a -> b) -> f a -> f b -}
type Fmap f = forall a b . (a -> b) -> f a -> f b

{- class Functor f => Applicative f where
    pure  :: a -> f a
    ap :: f (a -> b) -> f a -> f b -}
type Pure f = forall a . a -> f a
type Ap f = forall a b . f (a -> b) -> f a -> f b

data ApplicativeOp f = ApplicativeOp
  { _pure     :: Pure f
  , _ap       :: Ap f
  }

-- derive fmap from applicative operations
_fmap'A apOp = _ap apOp . _pure apOp

{- class Monad m where
       return :: a -> m a
       (>>=) :: m a -> (a -> m b) -> m b -}
type Bind m = forall a b . m a -> (a -> m b) -> m b

data MonadOp m = MonadOp
  { _bind         :: Bind m
  , _applicative  :: ApplicativeOp m
  }

-- derive applicative and functor operations from monad operations
_pure'M mOp = _pure $ _applicative mOp
_ap'M mOp = _ap $ _applicative mOp
_fmap'M mOp = _fmap'A $ _applicative mOp

-- helper function to create instance with pure and bind implementations
monadOp :: Pure m -> Bind m -> MonadOp m
monadOp pure bind = let
    (>>=) = bind
  in
  MonadOp
    { _bind = bind
    , _applicative = ApplicativeOp
      { _pure = pure
      , _ap = \m1 m2 -> m1 >>= \x1 -> m2 >>= \x2 -> pure (x1 x2)
      }
    }
