{-# LANGUAGE Rank2Types #-}

module Data.TraversableOp where

import Control.MonadOp
import Data.FoldableOp

{- class (Functor t, Foldable t) => Traversable t where
       traverse :: Applicative f => (a -> f b) -> t a -> f (t b) -}
type Traverse t = forall f a b . ApplicativeOp f -> (a -> f b) -> t a ->
  f (t b)

{- instance Traversable [] where
       traverse f = List.foldr cons_f (pure [])
         where cons_f x ys = liftA2 (:) (f x) ys -}
listTraverse aOp f = listFoldr (_ap aOp . _fmap'A aOp (:) . f) $ _pure aOp []
