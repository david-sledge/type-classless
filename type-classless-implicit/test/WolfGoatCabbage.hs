{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- addaptation of https://github.com/mstksg/inCode/blob/master/code-samples/monad-plus/WolfGoatCabbage.hs
module FarmerWolfGoatCabbage where

import Prelude hiding (all, fmap, foldMap, pure, (>>=), (<$>), (*>))
import Control.Monad.Misc
    ( ioApplicativeOps,
      ioMonadOps,
      listAlternativeOps,
      listApplicativeOps,
      listFunctorOps,
      listMonadOps )
import Control.MonadOps
    ( AlternativeOps,
      (*>),
      (>>=),
      alternativeApplicativeOps,
      alternativeMonoidLiftOps,
      empty,
      fmap,
      guardAnd,
      pure )
import Data.MonoidOps ( monoidAndOps )
import Data.TraversableOps
    ( all, foldMap, listFoldableOps, FoldableOps )

-- | Our types
data Character = Farmer | Wolf | Goat | Cabbage
    deriving (Show, Eq, Enum)

newtype Move = MoveThe Character
    deriving (Eq)

-- (just for debugging)
instance Show Move where
    show (MoveThe Farmer)  = "F"
    show (MoveThe Wolf)    = "W"
    show (MoveThe Goat)    = "G"
    show (MoveThe Cabbage) = "C"

type Plan = [Move]

data Position = West | East
    deriving (Show, Eq)

-- | Starting plan
startingPlan :: Plan
startingPlan = []

-- | Helper functions
positionOf :: Plan -> Character -> Position
positionOf p c =
  let positionFromCount n | even n    = West
                          | otherwise = East
  in
  positionFromCount . length $ case c of
    Farmer  -> p
    c       -> filter (== MoveThe c) p

moveLegal :: Plan -> Move -> Bool
moveLegal p (MoveThe Farmer)  = True
moveLegal p (MoveThe c)       = positionOf p c == positionOf p Farmer

moveRedundant :: Plan -> Move -> Bool
moveRedundant [] m' = False
moveRedundant p m   = last p == m

safePlan :: Plan -> Bool
safePlan p =
  let goatPos     = positionOf p Goat
      farmerPos   = positionOf p Farmer
      safeGoat    = goatPos /= positionOf p Wolf
      safeCabbage = positionOf p Cabbage /= goatPos
  in
  goatPos == farmerPos || safeGoat && safeCabbage

-- | One step of the journey: add a move.
makeMove :: Plan -> [Plan]
makeMove p =
  let ?monadOps = listMonadOps
      ?alternativeOps = listAlternativeOps
  in
  [Farmer ..] >>= (\ next ->
  let p' = p ++ [next] in
  guardAnd (moveLegal p next && not (moveRedundant p next) && safePlan p') p') . MoveThe

isSolution :: Plan -> Bool
isSolution p =
  let ?foldableOps = listFoldableOps
      ?functorOps = listFunctorOps
  in
  all (== East) $ fmap (positionOf p) [Farmer .. Cabbage]

-- | The full journey
findSolutions :: Int -> [Plan]
findSolutions n =
  let ?alternativeOps = listAlternativeOps
      ?applicativeOps = listApplicativeOps
      ?monadOps = listMonadOps
  in
  iterate (>>= makeMove) (pure startingPlan) !! n >>= \ p ->
  guardAnd (isSolution p) p
