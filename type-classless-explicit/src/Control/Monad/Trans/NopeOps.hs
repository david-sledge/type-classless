{-# LANGUAGE Rank2Types #-}

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
maybeTMonadOps :: MonadOps m -> MonadOps (MaybeT m)
maybeTMonadOps monadOps =
  let applicativeOps = _monadApplicativeOps monadOps in
  pkgMonadOps
    (MaybeT . _pure applicativeOps . Just) $
    \ x f ->
      MaybeT $ _bind monadOps (runMaybeT x) $ maybe (_pure applicativeOps Nothing) (runMaybeT . f)

type Nope m = forall a . m a
type Yup m = forall a . m a -> m a -> m a

data MonadNopeOps m = MonadNopeOps {
  _monadNopeMonadOps :: MonadOps m,
  _nope :: Nope m,
  _yup :: Yup m
}

-- monadNopeMonadOps :: (?monadNopeOps :: MonadNopeOps m) => MonadOps m
-- monadNopeMonadOps monadNopeOps = _monadNopeMonadOps monadNopeOps

-- nope :: (?monadNopeOps :: MonadNopeOps m) => Nope m
-- nope = _nope ?monadNopeOps

-- yup :: (?monadNopeOps :: MonadNopeOps m) => Yup m
-- yup = _yup ?monadNopeOps

liftMaybe :: MonadNopeOps m -> Maybe a -> m a
liftMaybe monadNopeOps =
  maybe (_nope monadNopeOps) . _pure . _monadApplicativeOps $ _monadNopeMonadOps monadNopeOps

tryNope :: MonadNopeOps m -> m a -> m (Maybe a)
tryNope monadNopeOps action =
  let applicativeOps = _monadApplicativeOps $ _monadNopeMonadOps monadNopeOps in
  _yup monadNopeOps (_fmap (_applicativeFunctorOps applicativeOps) Just action) $ _pure applicativeOps Nothing

handleNope :: MonadNopeOps m -> m a -> m a -> m a
handleNope monadNopeOps = flip $ _yup monadNopeOps

maybeTMonadNopeOps :: MonadOps m -> MonadNopeOps (MaybeT m)
maybeTMonadNopeOps monadOps =
  let pure = _pure $ _monadApplicativeOps monadOps in
  MonadNopeOps
    (maybeTMonadOps monadOps)
    (MaybeT $ pure Nothing) $
    \ m h -> MaybeT . _bind monadOps (runMaybeT m) . maybe (runMaybeT h) $ pure . Just

maybeMonadOps :: MonadOps (MaybeT Identity)
maybeMonadOps = maybeTMonadOps identityMonadOps
