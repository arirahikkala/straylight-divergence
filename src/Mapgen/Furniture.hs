{-# LANGUAGE TupleSections, DeriveDataTypeable, NamedFieldPuns, ParallelListComp, TemplateHaskell, FlexibleContexts, NoMonomorphismRestriction #-}
module Mapgen.Furniture where

import Mapgen.FurnitureCharacters

import Mapgen.Rect
import Mapgen.RectTree
import Mapgen.FindRectangles

import BasicTypes
import Tile
import Object
import Util
import Los

import AStarFFI (astar)

import Control.Monad.Random
import Control.Monad.Trans
import Data.Data
import Data.AdditiveGroup ((^+^), (^-^))
import Data.Array.IArray -- todo: Import qualified
import Data.Array.Unboxed
import Data.List (findIndex, mapAccumL, intercalate, delete, find, foldl')
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Exception (throw)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow

import Debug.Trace (trace)

import Data.Tuple

data RoomFunction = MeetingRoom deriving (Show, Eq)

rotateListL n xs = drop n xs ++ take n xs


-- | Break a contiguous, connected wall up into spans by openings.
wallSpans :: Ord a => Set a -> [a] -> [[a]]
wallSpans openings cs
    | Set.null openings = [cs] -- if there are no openings, the whole thing is a single span
    | otherwise = 
        let rotateToCoord = head $ Set.elems openings
            rotated = (\(x, y) -> drop 1 y ++ x) . break (==rotateToCoord) $ cs in
        filter (/= []) .
        foldPart (second (drop 1) . break (flip Set.member openings)) $ rotated  

-- convert doors on the fringe of a room (note that they never occur in the corners) into spaces that should be open in the inner fringe
-- a revolution in function naming: Make names so obscure and weird that they're memorable just by their badnesss
roomDoorSpaceTile :: BoundsRect -> Coord -> Maybe Coord
roomDoorSpaceTile r c@(Coord x y) = 
       case findIndex (c `elem`) $ wallEdges r of
          Just 0 -> Just $ Coord x (y + 1)
          Just 1 -> Just $ Coord (x - 1) y
          Just 2 -> Just $ Coord x (y - 1)
          Just 3 -> Just $ Coord (x + 1) y
          _ -> Nothing

-- opening coords should be inside the room, not within its walls (i.e. they should be within its innerFringe)
--blockedTile :: Rect -> [Coord] -> Coord -> Bool
-- allConnectingLines used here because it's a simple way to get some nice thickness to the connecting lines (if it turns out slow, well, that's fixable, but I doubt performance will be a problem)

blockedTiles room openings 
--    | trace ("room: " ++ show room ++ "\nopenings: " ++ show openings ++ "\n") False = undefined
    | otherwise =
        (listArray room $ repeat False) 
    // (map (, True) $ concat $ concat [allConnectingLines from to | from <- openings, to <- openings])

blockedTilesToObstacles :: Array Coord Bool -> [BoundsRect]
blockedTilesToObstacles a = map (id &&& (Coord 1 1 ^+^)) $ filter (fromMaybe False . (a !/)) $ range (bounds a)

--openRectangles :: Rect -> [Coord] -> [Rect]

--peel :: (Coord, Coord) -> (Coord, Coord)
--peel (Coord xStart yStart, Coord xEnd yEnd) = (Coord (xStart + 1) (yStart + 1), Coord (xEnd - 1) (yEnd - 1))

-- decompose a room into a greedy cover of open and closed rectangles, such that the closed rectangles cover the areas marked by drawing lines between every pair of entrances, and the open rectangles cover the rest
openAndClosedRectangles r openings = 
    let rInside = (id *** (^-^ Coord 1 1)) r
        closed' = filter ((>4) . boundsRectSize) .
                  findRectangles rInside .
                  blockedTilesToObstacles .
                  blockedTiles r .
                  mapMaybe (roomDoorSpaceTile $ r) $
                  openings
        open = filter ((>4) . boundsRectSize) $ findRectangles rInside closed' 
        closed = findRectangles rInside open in
    (open, closed)
--decorateLounge :: Rect -> [Coord] -> [(Location, Object)]

--decorateLobby
--  :: (Monad m, MonadRandom m, Functor m) => Array Coord Tile -> (Coord, (Int, Int)) -> [Coord] -> m [(Object, Coord)]

normalizeSizeRotation :: Coord -> Coord
normalizeSizeRotation c@(Coord x y) 
    | x < y = Coord y x
    | otherwise = c

decorateRect :: (Monad m, MonadRandom m, MonadIO m, Functor m, IArray a Tile) =>
                Map Coord Layouts
             -> a Coord Tile
             -> BoundsRect
             -> [Coord]
             -> m [(Object, Coord)]
decorateRect layouts a room@(upLeft, _) openings =
    do let allowed = filter (layoutAllowed a room openings)
                     $ concatMap layoutRotations
                     $ Map.findWithDefault [] (normalizeSizeRotation (snd room ^-^ fst room))
                     $ layouts
       pick <- uniformRandomPick $ allowed
       case pick of
         Nothing -> return []
         Just (x, _) ->
             return . map swap . justAssocs . map ((^+^ upLeft) *** fst) . assocs . flMap $ x

justAssocs :: [(a, Maybe b)] -> [(a, b)]
justAssocs [] = []
justAssocs ((a, Nothing):xs) = justAssocs xs
justAssocs ((a, Just x):xs) = (a, x) : justAssocs xs

decorateRoom layouts roomType a room openings = 
    let (open, closed) = openAndClosedRectangles room openings 
        allowed = Map.findWithDefault Map.empty roomType layouts
    in do
      openSubs <- map peel `fmap` (map fst . concat) `fmap` mapM (subdivideRectangle (campusSplit 3 6 5 0)) (map (bulgeDown room) open)
      closedSubs <- map peel `fmap` (map fst . concat) `fmap` mapM (subdivideRectangle (campusSplit 3 6 5 0)) (map (bulgeDown room) closed)
      openObjs <- mapM (\r -> decorateRect allowed a r openings) openSubs
      closedObjs <- mapM (\r -> decorateRect allowed a r openings) closedSubs
      return . concat $ (openObjs ++ closedObjs)

peel = second (^-^ Coord 1 1)
bulge = second (^+^ Coord 1 1)

bulgeDown b@(bb, be@(Coord bex bey)) x@(xb, xe@(Coord xex xey))
    | bex == succ xex && bey == succ xey = (xb, Coord (succ xex) (succ xey))
    | bex == succ xex = (xb, Coord (succ xex) xey)
    | bey == succ xey = (xb, Coord xex (succ xey))
    | otherwise = (xb, xe)

makeCubicleFarm minSize prefSize cs@(Coord xb yb, Coord xe ye)
    | xSizeOk && ySizeOk = return (X, [])
    | not xSizeOk = doSplit (xe - xb) X
    | not ySizeOk = doSplit (ye - yb) Y
    where doSplit axisSize axis =
              let (numSections, leftoverSize) = axisSize `divMod` prefSize in
              return $ (axis, 
                            if leftoverSize == 0
                            then replicate numSections prefSize
                            else if leftoverSize < minSize
                                 then snd $ mapAccumL addLeftovers (numSections, leftoverSize) $ replicate numSections prefSize
                                 else replicate numSections prefSize ++ [leftoverSize])
          xSizeOk = (xe - xb) <= (prefSize + 1)
          ySizeOk = (ye - yb) <= (prefSize + 1)

addLeftovers (numSectionsLeft, n) x =
    let added = div n numSectionsLeft + if mod n numSectionsLeft > 0 then 1 else 0 in
    ((pred numSectionsLeft, n - added), x + added)

testArray :: Array Coord Tile
testArray = lists2dToArray $ map (map (\c -> if c == '#' then makeWall PlainWall else makeFloor Concrete)) $ 
            [" # ###",
             "      ",
             " #    ",
             " #####"]

testLayout = FurnitureLayout
             (lists2dToArray $ map (map (\c -> if c == '#' then (Nothing, False) else (Nothing, True))) $ 
                             [" #",
                              "# "])
             (JustPlainWall, OpenSpace, AnyWall, OpenSpace)
             []

layoutAllowed :: IArray a Tile => 
                 a Coord Tile
              -> BoundsRect
              -> [Coord]
              -> FurnitureLayout
              -> Bool
--layoutAllowed a cs openings l | trace ("cs: " ++ show cs) False = undefined
layoutAllowed a cs openings l = 
    let ws = wallEdges cs
        toOrigin = (^-^ (fst $ cs))
        walkability :: UArray Coord Bool
--        walkability = amap snd $ flMap l
        walkability = array (bounds (flMap l)) $ map (second snd) $ assocs $ flMap l
        doorSpaces = mapMaybe (roomDoorSpaceTile cs) openings
        directionalSpaces = 
            map ((mapMaybe (roomDoorSpaceTile cs)) .
                 (filter (not . flip elem openings)) .
                 (filter (not . maybe False tileBlocksMovement . (a!/)))) $ ws
        checkEW = null (directionalSpaces !! 0) &&
                  null (directionalSpaces !! 2) &&
                  not (null (directionalSpaces !! 1)) &&
                  not (null (directionalSpaces !! 3))
        checkNS = null (directionalSpaces !! 1) &&
                  null (directionalSpaces !! 3) &&
                  not (null (directionalSpaces !! 0)) &&
                  not (null (directionalSpaces !! 2))
        ew | not checkEW = True
           | otherwise = any (not . null) $ [astar walkability (toOrigin from) (toOrigin to) | from <- directionalSpaces !! 1, to <- directionalSpaces !! 3]
        ns | not checkNS = True
           | otherwise = any (not . null) $ [astar walkability (toOrigin from) (toOrigin to) | from <- (directionalSpaces !! 0), to <- (directionalSpaces !! 2)]
        doors = all (\door -> any (not . null) $ [astar walkability (toOrigin door) (toOrigin to) | to <- concat directionalSpaces]) doorSpaces
        noBlockedOpenings = all (\d -> fromMaybe False (walkability !/ d)) $ doorSpaces
        -- currently limited to checking rectangular layouts within rectangular rooms: One wall tile is enough to satisfy a wall constraint
        -- (it *is* possible to have a situation where a big JustGlassWall constraint gets satisfied by one glass tile and lots of plain tiles, or vice versa
        -- but it *isn't* possible with the current layouts structure that a wall constraint gets satisfied by open space)
        checkWallConstraint c xs =
            case c of
              NoConstraint -> True
              AnyWall -> any (maybe False tileBlocksMovement . (a!/)) xs
              OpenSpace -> all (maybe False (not . tileBlocksMovement) . (a!/)) xs
              JustGlassWall -> any (maybe False ((GlassWall ==) . tileWallType) . (a!/)) xs
              JustPlainWall -> any (maybe False ((PlainWall ==) . tileWallType) . (a!/)) xs 
        wallConstraint = 
            case flWallConstraints l of
              (north, east, south, west) -> 
                  checkWallConstraint north (ws !! 0) &&
                  checkWallConstraint east (ws !! 1) &&
                  checkWallConstraint south (ws !! 2) &&
                  checkWallConstraint west (ws !! 3) in
--    trace (show (ew, ns, doors, wallConstraint, doorSpaces)) True &&
    ew && ns && doors && wallConstraint && noBlockedOpenings




coordContainedIn :: Coord -> BoundsRect -> Bool
coordContainedIn (Coord x y) (Coord xb yb, Coord xe ye) =
    x >= xb && x < xe && y >= yb && y < ye

exists :: [a] -> Bool
exists [] = False
exists _ = True

-- the walls around a room, including both those that it owns and those that it doesn't
-- in order (for the rectangle (Coord 0 0, Coord 2 2))
--  12
-- 7  3
-- 8  4
--  56
-- aka NESW, and intra-wall going top to down and left to right

wallEdges :: BoundsRect -> [[Coord]]
wallEdges (Coord xb yb, Coord xe ye) = 
    [[Coord x (yb - 1) | x <- [xb..xe - 1]],
     [Coord xe y | y <- [yb..ye - 1]],
     [Coord x ye | x <- [xb..xe - 1]],
     [Coord (xb - 1) y | y <- [yb..ye - 1]]]


-- constraints:
-- - matching size
-- - at least one tile that's adjacent to a layoutEntrance and not contained within the layout must be walkable
-- - wall constraints
-- ... and that's actually all there is to it; rotations are handled elsewhere, etc.
{-

Suppose we call makeCubicles such that prefSize is 7 and the chosen axis's length happens to be 10.

Solution (ish): Eh, fuck it. If that happens we make a [7, 3] sectioning. Add a minSize but no maxSize; if a section would end up smaller than minSize (i.e. axisSize `mod` (axisSize `div` prefSize) is greater than 0 but less than minSize), divide the extra section between the other ones (so, for instance, makeCubicles with minSize = 5, prefSize = 7, axisSize = 10 would section into [10]. Note that this implies (as with correct use of the split interface in general) that if you need to split in the *other* direction you'll want to do it first, because the split function won't be called again for a given subtree once you return a singleton result.

That means, then, that the axisOk predicate should be... axisSize <= (prefSize + 1)

-}


{-
Roiiiight, furniture definition is... taking... me... a... while...

But here's a plan:

- design a largish list of small-scale furniture layouts, maybe from 2x2 through 2x5 to 5x5 or so (so, 2x2, 2x3, 2x4, 2x5, 3x3, 3x4, 3x5, 4x4, 4x5, 5x5)
- each layout can have constraints on:
  - which room type it's found in
  - whether it's in the inside-connections set or the outside-connections set
  - whether its sides are against plain or glass walls (given rotations and flips, these options should be: AllPlain, TopGlass, TopAndLeftGlass, TopAndBottomGlass, TopLeftAndBottomGlass, AllGlass... but quite honestly I should be OK with just the first two)
- layouts provide the furniture types and the activity points

- parallelly, bludgeon the RectTree code into chopping up small areas into sets of 2x5 through 2x5 size (the binary tree nature should be quite beneficial too, as it allows for leaving nice orderly walking paths between furnitures)
- write a layout selector that tries to make appropriate semi-diverse choices from the list of furnitures that match the constraints


Well, surprisingly enough, I've made a scary amount of progress on this! Let's see what we have now:

- subdivideRectangle has come of age, and now provides a very powerful interface to write splitting functions against
 - one such function has already been written: makeCubicleFarm, which goes out of its way to make regular layouts (within the limits of integer division)
- layoutAllowed, the layout selector, exists; it's untested, though, and it's not known whether the simple-minded filter-the-whole-space approach will work if we end up with a whole lot of layout definitions (might be a better idea for performance to, say, index the layouts by size (normalised for rotations of course) and maybe whatever other criteria end up being important)
- layouts can be rotated and reflected and flipped and whathaveyou, so that they only have to be designed in one orientation; however, this only applies to square layouts (general rectangular layouts need a little bit more love... well, less love, since they have a couple fewer symmetries, but you get my point)
- also the layout data type looks good and the json library is doing a decent job helping me put instances of it in data files; activity points are already in, sort of, though managing furniture types will mean a little bit more code (therein most complexity will come from paranoia).

Stuff to work on once I get back to working on this:

- FurnitureLayouts aren't quite finished yet, they'll proly need a roomtype constraint; though on the other hand I guess roomtypes could refer to FurnitureLayouts instead
- try to test layoutAllowed to see it's not doing anything really incredibly stupid
- try to hook up the whole structure just to see if it works somehow

)
-}

data WallConstraint = NoConstraint | AnyWall | OpenSpace | JustGlassWall | JustPlainWall deriving (Data, Typeable, Show, Read, Eq)
data ActorType = Researcher | OfficeWorker deriving (Data, Typeable, Show, Read, Eq)
data ActivityType = UseWorkstation | GetStuffOnShelf deriving (Data, Typeable, Show, Read, Eq)

data ActivityPoint = ActivityPoint {
      apActivityType :: String
    , apLocationInLayout :: Coord
} deriving (Data, Typeable, Show, Read, Eq)

readFurnitureLayout :: [FurnitureCharacters] -> StoredFurnitureLayout -> Map String FurniturePrototype -> FurnitureLayout
readFurnitureLayout characters (StoredFurnitureLayout c m w a r) prototypes =
    let arr = lists2dToArray m 
        cdefs = case find ((==c).charactersName) characters of
                  Nothing -> throw $ DataFormatException ("couldn't find furniture type " ++ c)
                  Just x -> x in
    FurnitureLayout (amap (instantiateCharacter cdefs prototypes) (lists2dToArray m)) w a

indexPrototypes :: [FurniturePrototype] -> Map String FurniturePrototype
indexPrototypes = foldl' (\m e -> Map.insert (furniturePrototypeName e) e m) Map.empty

data FurnitureLayout = FurnitureLayout {
      flMap :: Array Coord (Maybe Object, Bool)
    , flWallConstraints :: (WallConstraint, WallConstraint, WallConstraint, WallConstraint)
    , flActivityPoints :: [ActivityPoint]
} deriving (Show)

data StoredFurnitureLayout = StoredFurnitureLayout {
      layoutCharacters :: String
    , layoutMap :: [String]
    , layoutWallConstraints :: (WallConstraint, WallConstraint, WallConstraint, WallConstraint)

    , layoutActivityPoints :: [ActivityPoint]
    , layoutRooms :: [String]
} deriving (Data, Typeable, Show, Read, Eq)

modActivityPointCoord f ap@(ActivityPoint {apLocationInLayout}) = 
    ap { apLocationInLayout = f apLocationInLayout }

layoutRotations :: FurnitureLayout -> [FurnitureLayout]
layoutRotations l@(FurnitureLayout {flMap, flWallConstraints, flActivityPoints}) =
    let rect@(Coord rx ry) = snd $ bounds flMap
        layoutIsSquare = rx == ry
    in [let convertCoord = (!!n) . coordRotations (snd $ bounds flMap) in
        l { flMap = ixmap (bounds flMap) convertCoord flMap
          , flWallConstraints = convertConstraints flWallConstraints
          , flActivityPoints = map (modActivityPointCoord convertCoord) flActivityPoints }
            | n <- [0.. if layoutIsSquare then 7 else 3]
            | convertConstraints <- [(\(n, e, s, w) -> (n, e, s, w)), -- id
                                     (\(n, e, s, w) -> (s, w, n, e)), -- rotate180
                                     (\(n, e, s, w) -> (s, e, n, w)), -- flipvert
                                     (\(n, e, s, w) -> (n, w, s, e)), -- fliphorz
                                     (\(n, e, s, w) -> (w, n, e, s)), -- rotate90
                                     (\(n, e, s, w) -> (e, s, w, n)), -- rotate270
                                     (\(n, e, s, w) -> (e, n, w, s)), -- flipur
                                     (\(n, e, s, w) -> (w, s, e, n))]]-- flipul


coordRotations r@(Coord rx ry) c = [c, rotate180 c, flipvert c, fliphorz c, rotate90 c, rotate270 c, flipur c, flipul c]
    where 
      rotate270 (Coord x y) = Coord (ry - y) x
      rotate180 (Coord x y) = Coord (rx - x) (ry - y)
      rotate90 (Coord x y) = Coord y (rx - x)
      flipvert (Coord x y) = Coord x (ry - y)
      fliphorz (Coord x y) = Coord (rx - x) y
      flipur (Coord x y) = Coord y x
      flipul (Coord x y) = Coord (ry - y) (rx - x)

testLayouts = [StoredFurnitureLayout {layoutCharacters = "office", layoutMap = ["p.cc.","$_...",".....","$_...","`..ss"], layoutWallConstraints = (NoConstraint,NoConstraint,JustPlainWall,JustPlainWall), layoutActivityPoints = [ActivityPoint {apActivityType = "use workstation", apLocationInLayout = Coord {x = 1, y = 1}},ActivityPoint {apActivityType = "use workstation", apLocationInLayout = Coord {x = 1, y = 3}}], layoutRooms = []}]



type Layouts = [FurnitureLayout]
