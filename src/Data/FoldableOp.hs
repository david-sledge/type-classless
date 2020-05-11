{-# LANGUAGE Rank2Types #-}

module Data.FoldableOp where

type Fold t = forall a b . (a -> b -> b) -> b -> t a -> b

{- class Foldable t where
       foldr :: (a -> b -> b) -> b -> t a -> b -}
newtype FoldableOp t = FoldableOp {
    _foldr :: Fold t
  }

foldable'ListOp :: FoldableOp []
foldable'ListOp = FoldableOp {
    _foldr = foldr
  }
