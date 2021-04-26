{-# LANGUAGE Rank2Types #-}

module Data.MonoidOp where

type Assoc a = a -> a -> a

-- a monoid has an associative operation with an identity with the operation
data MonoidOp a = MonoidOp
  { _assoc :: Assoc a
  , _mempty :: a
  }

-- numbers are monoids with respect to more than one associative operation
--MonoidOp Integer
monoidSumOp = MonoidOp (+) 0
--MonoidOp Integer
monoidProductOp = MonoidOp (*) 1

-- so are Booleans
--MonoidOp Bool
monoidOrOp = MonoidOp (||) False
--MonoidOp Bool
monoidAndOp = MonoidOp (&&) True
