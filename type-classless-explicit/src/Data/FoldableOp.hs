{-# LANGUAGE Rank2Types #-}

module Data.FoldableOp (Fold, listFoldr) where

{- class Foldable t where
       foldr :: (a -> b -> b) -> b -> t a -> b -}
type Fold t = forall a b . (a -> b -> b) -> b -> t a -> b

listFoldr :: Fold []
listFoldr = foldr
