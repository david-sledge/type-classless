{-# LANGUAGE Rank2Types #-}

module Control.Monad.PlusOp where

--import Data.Function
import Control.MonadOp
import Data.MonoidOp

-- any type that's both a monoid and an applicative is an alternative. The
-- minimal functions Alternative require (empty, and <|>) are already fulfilled
-- by the required functions of Monoid. Beyond convenience there is no new
-- functionality.
type ApId f = forall a . f a
type ApAssoc f = forall a. Assoc (f a)

alternativeOp ::
  ApAssoc f -> ApId f -> Pure f -> Ap f -> (ApplicativeOp f, MonoidOp (f a))
alternativeOp assoc mempty pure ap =
  (ApplicativeOp pure ap, MonoidOp assoc mempty)

-- any type that's both monad and a monoid is a monad plus. Everything that
-- applies to AlternativeOp with regard to functionality is true for MonadPlus,
-- too. Beyond convenience no new fuctionality to define.
-- This function produces 7 typeclasses in one: MonadPlus, Monad, Applicative,
-- Functor, Alternative, Monoid, and Semigroup
monadPlusOp ::
  ApAssoc m -> ApId m -> Pure m -> Bind m -> (MonadOp m, MonoidOp (m a))
monadPlusOp assoc mempty pure bind = (monadOp pure bind, MonoidOp assoc mempty)
