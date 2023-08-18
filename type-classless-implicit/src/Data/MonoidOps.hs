{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Data.MonoidOps where
import Data.Monoid (Endo(Endo))

type Assoc a = a -> a -> a

-- a monoid is an associative operation with an identity value
data MonoidOps a = MonoidOps
  { _assoc :: Assoc a
  , _ident :: a
  }

assoc :: (?monoidOps :: MonoidOps a) => Assoc a
assoc = _assoc ?monoidOps

(<>) :: (?monoidOps :: MonoidOps a) => Assoc a
(<>) = assoc

ident :: (?monoidOps :: MonoidOps a) => a
ident = _ident ?monoidOps

-- numbers are monoids with respect to more than one associative operation
--MonoidOp Integer
monoidSumOps :: MonoidOps Integer
monoidSumOps = MonoidOps (+) 0
--MonoidOp Integer
monoidProductOps :: MonoidOps Integer
monoidProductOps = MonoidOps (*) 1

-- so are Booleans
--MonoidOp Bool
monoidOrOps :: MonoidOps Bool
monoidOrOps = MonoidOps (||) False
--MonoidOp Bool
monoidAndOps :: MonoidOps Bool
monoidAndOps = MonoidOps (&&) True

{- instance Monoid (Endo a) where
        mempty = Endo id -}
monoidEndo :: MonoidOps (Endo a)
monoidEndo = MonoidOps (Prelude.<>) $ Endo id

listMonoidOps :: MonoidOps [a]
listMonoidOps = MonoidOps (++) []
