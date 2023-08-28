{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.TraversableOps where

import Prelude hiding (pure, fmap, foldMap, foldr, traverse)
import Control.MonadOps (fmap, liftA2, pure, ApplicativeOps, FunctorOps)
import Control.Monad.Misc (listFunctorOps, constApplicativeOps)
import Data.MonoidOps (assoc, ident, monoidAndOps, monoidEndoOps, monoidOrOps, MonoidOps)
import Control.Applicative (getConst, Const (Const))

type Foldr t = forall a b . (a -> b -> b) -> b -> t a -> b
type FoldMap t = forall a m . (?monoidOps :: MonoidOps m) => (a -> m) -> t a -> m

{- class Foldable t where
       foldr :: (a -> b -> b) -> b -> t a -> b
       foldMap :: Monoid m => (a -> m) -> t a -> m -}
data FoldableOps t = FoldableOps {
  _foldr :: Foldr t,
  _foldMap :: FoldMap t
}

foldr :: (?foldableOps :: FoldableOps t) => Foldr t
foldr = _foldr ?foldableOps

foldMap :: (?foldableOps :: FoldableOps t) => FoldMap t
foldMap = _foldMap ?foldableOps

pkgFoldableOps :: Either (Foldr t, FoldMap t) (Either (Foldr t) (FoldMap t)) -> FoldableOps t
pkgFoldableOps (Left (foldrF, foldMapF)) = FoldableOps foldrF foldMapF
pkgFoldableOps (Right (Left foldrF)) = FoldableOps foldrF $ \ f -> foldrF (assoc . f) ident
pkgFoldableOps (Right (Right foldMapF)) = FoldableOps (let ?monoidOps = monoidEndoOps in flip . foldMapF) foldMapF

all :: (?foldableOps :: FoldableOps t) => (a -> Bool) -> t a -> Bool
all = let ?monoidOps = monoidAndOps in foldMap

any :: (?foldableOps :: FoldableOps t) => (a -> Bool) -> t a -> Bool
any = let ?monoidOps = monoidOrOps in foldMap

type Traverse t = forall f a b . (?applicativeOps :: ApplicativeOps f) =>
  (a -> f b) -> t a -> f (t b)
type SequenceA t = forall f a . (?applicativeOps :: ApplicativeOps f) =>
  t (f a) -> f (t a)

{- class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id -}
data TraversableOps t = TraversableOps {
  _traversableFunctorOps :: FunctorOps t,
  _traverse :: Traverse t,
  _sequenceA :: SequenceA t,
  _traversableFoldableOps :: FoldableOps t
}

traverse :: (?traversableOps :: TraversableOps t) => Traverse t
traverse = _traverse ?traversableOps

sequenceA :: (?traversableOps :: TraversableOps t) => SequenceA t
sequenceA = _sequenceA ?traversableOps

-- generalized version of: (?functorOps :: FunctorOps t) => SequenceA t -> Traverse t
sequenceATraverse :: (?functorOps :: FunctorOps f) => (f b -> c) -> (a -> b) -> f a -> c
sequenceATraverse sequenceAF = (sequenceAF .) . fmap

-- generalized version of: Traverse t -> SequenceA t
traverseSequenceA :: ((a -> a) -> b) -> b
traverseSequenceA = ($ id)

-- derive Foldable from traverse
traversableFoldableOps :: Traverse t -> FoldableOps t
traversableFoldableOps traverseF = pkgFoldableOps . Right . Right $
  let ?applicativeOps = constApplicativeOps in
  (getConst .) . traverseF . (Const .)

pkgTraversableOps :: (?functorOps :: FunctorOps t) => Either (Traverse t, SequenceA t) (Either (Traverse t) (SequenceA t)) -> TraversableOps t
pkgTraversableOps =
  let f = TraversableOps ?functorOps in
  either
    (\ (traverseF, sequenceAF) -> f traverseF sequenceAF $ traversableFoldableOps traverseF)
    (either
      (\ traverseF -> f traverseF (traverseF id) $ traversableFoldableOps traverseF)
      (\ sequenceAF -> f (sequenceATraverse sequenceAF) sequenceAF . traversableFoldableOps $ sequenceATraverse sequenceAF))

{- instance Traversable [] where
       traverse f = List.foldr cons_f (pure [])
         where cons_f x ys = liftA2 (:) (f x) ys -}
-- Because Foldable can be derived from traversable (and foldr by inclusion),
-- let's not use foldr for this definition to show that Foldable is not
-- actually a constraint.
listTraversableOps :: TraversableOps []
listTraversableOps =
  let ?functorOps = listFunctorOps in
  pkgTraversableOps . Right . Left $
    let go f (x : xs) = liftA2 (:) (f x) (go f xs)
        go _ _ = pure []
    in go

listFoldableOps :: FoldableOps []
listFoldableOps = pkgFoldableOps . Right . Left $
{- foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys -}
  \ k z ->
    let go [] = z
        go (y : ys) = y `k` go ys
    in go

{- instance Foldable Maybe where
  foldMap = maybe mempty

  foldr _ z Nothing = z
  foldr f z (Just x) = f x z -}
maybeFoldableOps :: FoldableOps Maybe
maybeFoldableOps = pkgFoldableOps $ Left (\ k b -> maybe b (`k` b), maybe ident)
