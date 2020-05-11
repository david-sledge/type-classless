{-# LANGUAGE Rank2Types #-}

module Data.SemigroupOp where

type Assoc a = a -> a -> a

-- a semigroup is a type with an associative binary operation
newtype SemigroupOp a = SemigroupOp {
    _assoc :: Assoc a }
