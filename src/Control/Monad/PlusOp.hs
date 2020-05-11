{-# LANGUAGE Rank2Types #-}

module Control.Monad.PlusOp where

--import Data.Function
import Control.MonadOp
import Data.MonoidOp
import Data.SemigroupOp

-- any type that's both a monoid and an applicative is an alternative. The
-- minimal functions Alternative require (empty, and <|>) are already fulfilled
-- by the required functions of Monoid. There is no new functionality.
type ApId f = forall a . f a
type ApAssoc f = forall a. Assoc (f a)

alternativeOp ::
  ApAssoc f -> ApId f -> Pure f -> Ap f -> (ApplicativeOp f, MonoidOp (f a))
alternativeOp assoc mempty pure ap =
  (applicativeOp pure ap, monoidOp assoc mempty)

-- any type that's both monad and a monoid is a monad plus. Everything that
-- applies to AlternativeOp with regard to functionality is true for MonadPlus,
-- too. No new fuctionality to define.
monadPlusOp ::
  ApAssoc m -> ApId m -> Pure m -> Bind m -> (MonadOp m, MonoidOp (m a))
monadPlusOp assoc mempty pure bind = (monadOp pure bind, monoidOp assoc mempty)

-- 7 in one: MonadPlus, Monad, Applicative, Functor, Alternative, Monoid, and
-- Semigroup
--monadPlus'MaybeOp :: MonadPlusOp Maybe
monadPlus'MaybeOp = monadPlusOp (
      \ma mb ->
        case ma of
        Nothing -> mb
        _ -> ma )
    Nothing Just $ \mx f ->
      case mx of
      Just x -> f x
      _ -> Nothing

--monoid'MaybeOp :: MonoidOp (Maybe a)
monoid'MaybeOp = snd $ monadPlus'MaybeOp

--monad'MaybeOp :: MonadOp Maybe
monad'MaybeOp = fst $ monadPlus'MaybeOp
