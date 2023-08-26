{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Control.Monad.Trans.NopeOps where

import Prelude hiding (pure, (>>=), fmap, (<$>))
import Control.Monad.Trans.Maybe
import Control.MonadOps
import Control.Monad.Identity (Identity)
import Control.Monad.Misc

{- instance (Monad m) => Monad (MaybeT m) where
    return = MaybeT . return . Just

    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y) -}
maybeTMonadOps :: (?monadOps :: MonadOps m) => MonadOps (MaybeT m)
maybeTMonadOps =
  let ?applicativeOps = monadApplicativeOps in
  pkgMonadOps
    (MaybeT . pure . Just) $
    \ x f ->
      MaybeT $ runMaybeT x >>= maybe (pure Nothing) (runMaybeT . f)

type Nope m = forall a . m a
type Yup m = forall a . m a -> m a -> m a

data MonadNopeOps m = MonadNopeOps {
  _monadNopeMonadOps :: MonadOps m,
  _nope :: Nope m,
  _yup :: Yup m
}

monadNopeMonadOps :: (?monadNopeOps :: MonadNopeOps m) => MonadOps m
monadNopeMonadOps = _monadNopeMonadOps ?monadNopeOps

nope :: (?monadNopeOps :: MonadNopeOps m) => Nope m
nope = _nope ?monadNopeOps

yup :: (?monadNopeOps :: MonadNopeOps m) => Yup m
yup = _yup ?monadNopeOps

liftMaybe :: (?monadNopeOps :: MonadNopeOps m) => Maybe a -> m a
liftMaybe =
  let ?monadOps = monadNopeMonadOps in
  let ?applicativeOps = monadApplicativeOps in
  maybe nope pure

tryNope :: (?monadNopeOps :: MonadNopeOps m) => m a -> m (Maybe a)
tryNope action =
  let ?applicativeOps = _monadApplicativeOps monadNopeMonadOps in
  let ?functorOps = applicativeFunctorOps in
  (Just <$> action) `yup` pure Nothing

handleNope :: (?monadNopeOps :: MonadNopeOps m) => m a -> m a -> m a
handleNope = flip yup

maybeTMonadNopeOps :: (?monadOps :: MonadOps m) => MonadNopeOps (MaybeT m)
maybeTMonadNopeOps =
  let ?applicativeOps = monadApplicativeOps in
  MonadNopeOps
    maybeTMonadOps
    (MaybeT $ pure Nothing) $
    \ m h -> MaybeT $ runMaybeT m >>= maybe (runMaybeT h) (pure . Just)

maybeMonadOps :: MonadOps (MaybeT Identity)
maybeMonadOps = let ?monadOps = identityMonadOps in maybeTMonadOps
