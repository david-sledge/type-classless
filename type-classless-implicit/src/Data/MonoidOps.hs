{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImplicitParams #-}

module Data.MonoidOps where

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

listMonoidOps :: MonoidOps [a]
listMonoidOps = MonoidOps (++) []

{-
instance Semigroup a => Semigroup [a] where
  (x:xs) <> (y:ys) = x <> y : xs <> ys
  [] <> xs = xs
  xs <> _ = xs

instance Semigroup a => Monoid [a] where
  mempty = []
--}
assocListMonoidOps :: Assoc a -> MonoidOps [a]
assocListMonoidOps assocF =
  MonoidOps
    ( let g accf (x : xs) (y : ys) = g (accf . (assocF x y:)) xs ys
          g accf xs@(_ : _) _ = accf xs
          g accf _ ys = accf ys
      in g id
    ) []

zipListMonoidOps :: MonoidOps [a]
zipListMonoidOps = MonoidOps (\ xs ys -> xs ++ drop (length xs) ys) []

{- instance Monoid (Endo a) where
        mempty = Endo id -}
monoidEndoOps :: MonoidOps (a -> a)
monoidEndoOps = MonoidOps (.) id
