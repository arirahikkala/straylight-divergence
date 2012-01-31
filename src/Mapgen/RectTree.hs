{-# LANGUAGE ParallelListComp, TupleSections, NamedFieldPuns, ScopedTypeVariables, NoMonomorphismRestriction #-}
module Mapgen.RectTree (roomMap, defaultRectTreeArgs, RectTreeArgs (RectTreeArgs), Axis(..), campusSplit, subdivideRectangle) where

{- Todo:
  - customizablify. RoomTreeArgs currently allows for minSize to be customised. I'd say that at least the grow, pickSectionAmount, and fitSections functions should go into args so they can be customised. 
-}

import Debug.Trace (trace)

import Data.Array.IArray

import Control.Monad.Random (MonadRandom)
import Control.Monad.Random.Class (getRandomR)

import Util (probOutOf, weightedRandomPick, applyOnIndex, (!/)) 
import Rect
import Mapgen.Rect

import Data.Maybe
import Control.Monad.State
import System.Random (Random, random, randomR) 
import BasicTypes

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (nub, intercalate, mapAccumL)

import Data.AdditiveGroup

import Data.Graph.Inductive

import Mapgen.Util

import Mapgen.Rect

import qualified Control.Arrow as Arrow

instance Def RectTreeArgs where
    def = defaultRectTreeArgs

-- | Generate an irregular mesh of rectangular rooms covering an area of the given size
-- | Returns (array of which coord is within which room (number), map from room numbers to Rooms)
-- |
-- | A room contains its bottom and right walls, but not its top and left walls. Example:
-- | 
-- | 1122
-- | 1122
-- |  33
-- |  33
-- | 
-- | This arrangement of rooms corresponds to this arrangement of floors and walls:
-- | 
-- | .#.#
-- | ####
-- |  .#
-- |  ##
-- |
-- | Note how there is space to connect rooms 1 and 2 with a door, but not room 3 (without putting a door in a corner, which this mapgen won't do).

-- nb: currently ignores the RectTreeArgs argument
roomMap
  :: (MonadRandom m,
      MonadIO m,
      Functor m) =>
     (Coord, Coord)
     -> RectTreeArgs
     -> m (Array Coord Node, Gr BoundsRect ())

roomMap bounds args = 
    do roomList <- (zip [0..] . map fst) `fmap` subdivideRectangle (campusSplit 5 20 50 60) bounds
       let a = array (Arrow.second (^-^ (Coord 1 1)) $ bounds) $ concatMap (\(n, cs) -> map (,n) $ rectCoords cs) $ roomList 
           roomGraph = undir $ makeRoomGraph roomList a
       return (a, roomGraph)


data RectTreeArgs = RectTreeArgs {
      minSize :: Int
    , maxSize :: Int
}

defaultRectTreeArgs = RectTreeArgs 4 14

data Axis = X | Y deriving (Show, Eq)
{-
instance Random Axis where
    random g = let (a, newG) = random g in
               (if (a :: Bool) then X else Y, newG)
    randomR _ _ = error "randomR for Random Axis not implemented"
-}

pickSectionAmount = go `fmap` getRandomR (1, 1000 :: Int)
    where go = const 2 
{-    where go x | x <= 900 = 2
               | x <= 950 = 3
               | x <= 990 = 5
               | x <= 999 = 7
               | otherwise = 11
-}

-- todo: handle decreasing the numSections that came from pickSectionAmount in case axisSize > maxSize && (axisSize `div` numSections) <= minSize
-- also note that if minSize > maxSize `div` 2, campusSplit might leave sections bigger than maxSize around
-- current splitting function for subdivideRectangle used by the Campus mapgen
campusSplit minSize maxSize sizeNumerator sizeDenominator cs@(Coord xb yb, Coord xe ye) _ =
    let xSize = xe - xb
        ySize = ye - yb in
    do r <- getRandomR (0.75, 1.25 :: Double)
       let axis' = if r * fromIntegral xSize > fromIntegral ySize then X else Y
           -- switch axis if the other one is over maxSize and the random pick wasn't
           axis | axis' == X && xSize <= maxSize && ySize > maxSize = Y
                | axis' == Y && ySize <= maxSize && xSize > maxSize = X
                | otherwise = axis'
           axisSize = case axis of
                        X -> xSize
                        Y -> ySize
       stopHere <- sizeNumerator `probOutOf` (xSize + ySize + sizeDenominator)
       numSections <- pickSectionAmount
       sections <- fitSections axisSize numSections minSize
       case ((axisSize > maxSize) || stopHere) && ((axisSize `div` numSections) >= minSize) of
         True -> return (axis, sections)
         False -> return (X, [])

fitSections fullSize numSections minSize
    | numSections * minSize > fullSize = return []
fitSections fullSize 2 minSize = do
  firstSize <- getRandomR (minSize, fullSize - minSize)
  return $ [firstSize, fullSize - firstSize]
fitSections fullSize numSections _ = do
  let (usualSize, oddOnesSize) = divMod fullSize numSections
  return $ ((if 0 /= oddOnesSize 
             then ((oddOnesSize + usualSize) :) 
             else id) $ 
            replicate (if 0 /= oddOnesSize 
                       then numSections - 1 
                       else numSections) usualSize)

-- ^ Divide a given rectangle into a cover of smaller rectangles according to the given RectTreeArgs rule. Returns a list of subrectangles plus a unique path leading down to subrectangles such that sets of subrectangles sharing path postfixes are generally "close" to each other (and the set of all subrectangles sharing a given path postfix always covers a rectangle)
-- ^ Note "postfixes" - map (Arrow.second reverse) might make this function's results easier to reason about
subdivideRectangle :: forall m.
                      (Monad m, Functor m) =>
                      (BoundsRect -> [Int] -> m (Axis, [Int]))
                   -> BoundsRect
                   -> m [(BoundsRect, [Int])]
subdivideRectangle split coords@(Coord xBegin yBegin, Coord xEnd yEnd) =
    go (coords, [])
        where 
          go (cs@(Coord xb yb, Coord xe ye), path) =
              do (axis, s) <- split cs path
                 case s of
                   [] -> return [(cs, path)]
                   (_:[]) -> return [(cs, path)]
                   sections ->
                       let (aBegin, makeCoord) = 
                               case axis of
                                 X -> (xb, \ab ae -> (Coord ab yb, Coord ae ye))
                                 Y -> (yb, \ab ae -> (Coord xb ab, Coord xe ae))
                           sectCoords = scanl (+) aBegin sections in
                       concat `fmap` mapM go [(makeCoord ab ae, p : path) | ab <- init sectCoords | ae <- tail sectCoords | p <- [0..]]


isTopLeftCornerOfARoom :: Array Coord Int -> Coord -> Bool
isTopLeftCornerOfARoom a c@(Coord x y) = 
    let n = a !/ c in
    (n /= (a !/ (Coord (x - 1) y)) && n /= (a !/ (Coord x (y - 1))))

isArrayBottomRightEdge :: Array Coord Int -> Coord -> Bool
isArrayBottomRightEdge a c@(Coord x y) =
    let (_, (Coord xMax yMax)) = bounds a in
    (x == xMax || y == yMax)

findConnectedRooms :: Array Coord Int -> BoundsRect -> [Int]
findConnectedRooms a = 
    nub .
    mapMaybe (a !/) .
    filter (not . isArrayBottomRightEdge a) .
    filter (not . isTopLeftCornerOfARoom a) . 
    connectiveTiles



--makeRoomGraph :: [(Int, Rect)] -> Array Coord Int -> Gr Rect ()
makeRoomGraph rs a = 
    let m = Map.fromList rs in
    mkGraph rs $ [(from, to, ()) | from <- map fst rs, to <- findConnectedRooms a (m Map.! from)]
