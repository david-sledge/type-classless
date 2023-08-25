{-# LANGUAGE ImpredicativeTypes #-}

module Data.TraversableOps where

import Prelude hiding (pure, fmap, foldr, traverse)
import Control.MonadOps
import Control.Monad.Misc (listFunctorOps, constApplicativeOps)
import Data.MonoidOps
import Data.Monoid (appEndo, Endo (Endo))
import Control.Applicative (getConst, Const (Const))

type Foldr t = forall a b . (a -> b -> b) -> b -> t a -> b
type FoldMap t = forall a m . MonoidOps m -> (a -> m) -> t a -> m

{- class Foldable t where
       foldr :: (a -> b -> b) -> b -> t a -> b
       foldMap :: Monoid m => (a -> m) -> t a -> m -}
data FoldableOps t = FoldableOps {
  _foldr :: Foldr t,
  _foldMap :: FoldMap t
}

-- foldr :: (?foldableOps :: FoldableOps t) => Foldr t
-- foldr = _foldr ?foldableOps

-- foldMap :: (?foldableOps :: FoldableOps t) => FoldMap t
-- foldMap = _foldMap ?foldableOps

pkgFoldableOps :: Either (Foldr t, FoldMap t) (Either (Foldr t) (FoldMap t)) -> FoldableOps t
pkgFoldableOps (Left (foldrF, foldMapF)) = FoldableOps foldrF foldMapF
pkgFoldableOps (Right (Left foldrF)) = FoldableOps foldrF $ \ monoidOps f -> foldrF (_assoc monoidOps . f) $ _ident monoidOps
pkgFoldableOps (Right (Right foldMapF)) = FoldableOps (\ f z t -> appEndo (foldMapF monoidEndo (Endo . f) t) z) foldMapF

type Traverse t = forall f a b . ApplicativeOps f -> (a -> f b) -> t a -> f (t b)
type SequenceA t = forall f a . ApplicativeOps f -> t (f a) -> f (t a)

{- class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id -}
data TraversableOps t = TraversableOps {
  _traverse :: Traverse t,
  _sequenceA :: SequenceA t,
  _traversableFunctorOps :: FunctorOps t,
  _traversableFoldableOps :: FoldableOps t
}

-- traverse :: (?traversableOps :: TraversableOps t) => Traverse t
-- traverse = _traverse ?traversableOps

-- sequenceA :: (?traversableOps :: TraversableOps t) => SequenceA t
-- sequenceA = _sequenceA ?traversableOps

-- generalized version of: (?functorOps :: FunctorOps t) => SequenceA t -> Traverse t
sequenceATraverse :: FunctorOps f -> (f b -> c) -> (a -> b) -> f a -> c
sequenceATraverse functorOps sequenceAF = (sequenceAF .) . _fmap functorOps

-- generalized version of: Traverse t -> SequenceA t
-- traverseSequenceA :: ((a -> a) -> b) -> b
-- traverseSequenceA = ($ id)

-- derive Foldable from traverse
traversableFoldableOps :: Traverse t -> FoldableOps t
traversableFoldableOps traverseF = pkgFoldableOps . Right . Right $
  \ monoidOps -> (getConst .) . traverseF (constApplicativeOps monoidOps) . (Const .)

pkgTraversableOps :: FunctorOps t -> Either (Traverse t, SequenceA t) (Either (Traverse t) (SequenceA t)) -> TraversableOps t
pkgTraversableOps functorOps (Left (traverseF, sequenceAF)) = TraversableOps traverseF sequenceAF functorOps $ traversableFoldableOps traverseF
pkgTraversableOps functorOps (Right (Left traverseF)) = TraversableOps traverseF (`traverseF` id) functorOps $ traversableFoldableOps traverseF
pkgTraversableOps functorOps (Right (Right sequenceAF)) =
  let traverseF = sequenceATraverse functorOps . sequenceAF in
  TraversableOps traverseF sequenceAF functorOps $ traversableFoldableOps traverseF

{- instance Traversable [] where
       traverse f = List.foldr cons_f (pure [])
         where cons_f x ys = liftA2 (:) (f x) ys -}
-- Because Foldable can be derived from traversable (and foldr by inclusion),
-- let's not use foldr for this definition to show that Foldable is not
-- actually a constraint.
listTraversableOps :: TraversableOps []
listTraversableOps =
  pkgTraversableOps listFunctorOps . Right . Left $ \ applicativeOps ->
    let go f (x : xs) = _liftA2 applicativeOps (:) (f x) (go f xs)
        go _ _ = _pure applicativeOps []
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
maybeFoldableOps = pkgFoldableOps $ Left (\ k b -> maybe b (`k` b), maybe . _ident)
