{-# LANGUAGE Rank2Types #-}

module Data.MonoidOp where

import Data.SemigroupOp

-- a monoid is a semigroup with an identity
data MonoidOp a = MonoidOp {
    _mempty :: a,
    _semigroup'MoOp :: SemigroupOp a,
    _assoc'Mo    :: Assoc a }

--monoidOp :: Assoc a -> a -> MonoidOp a
monoidOp assoc mempty =
  MonoidOp {
    _semigroup'MoOp = SemigroupOp assoc,
    _mempty = mempty,
    _assoc'Mo = assoc
  }

-- numbers are monoids with respect to more than one associative operation
--MonoidOp Integer
monoidSumOp = monoidOp (+) 0
--MonoidOp Integer
monoidProductOp = monoidOp (*) 1

-- so are Booleans
--MonoidOp Bool
monoidOrOp = monoidOp (||) False
--MonoidOp Bool
monoidAndOp = monoidOp (&&) True
