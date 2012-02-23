{-# LANGUAGE TemplateHaskell, TupleSections, NoMonomorphismRestriction, NamedFieldPuns, FlexibleContexts, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification #-}
module Mapgen.Room where

import Data.Accessor
import Data.Accessor.Template

import Data.Graph.Inductive

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Random.Class
import Control.Arrow ((&&&), (***), second)
import qualified Control.Arrow as Arrow (first, second)
import System.Random
import Data.List
import Data.Array.IArray
import Data.Maybe (mapMaybe, catMaybes)
import Data.Data

import Data.Map (Map)
import qualified Data.Map as Map

import Mapgen.Rect
import Mapgen.Util (justLab)
import Mapgen.Furniture
import Mapgen.FurnitureCharacters
import Mapgen.CampusArgs
import BasicTypes
import CommonTypes
import Tile
import Object
import Util (cfoldr, uniformRandomPick, justUniformRandomPick, matching, gmapDouble)
import SaveLoad (loadDataFromFile)

import Text.Printf (printf)
import Debug.Trace (trace)
import Data.VectorSpace
import Control.Exception (evaluate)
import Data.Generics.SYB.WithClass.Derive

data Connectivity = None | OneDoor | Corridor Int deriving (Eq, Ord, Show)

isCorridor (Corridor _) = True
isCorridor _ = False

data EdgeLabel = EdgeLabel {
      connectivity_ :: Connectivity
} deriving (Eq, Ord, Show)

data NodeLabel = RoomLabel {
      room_ :: BoundsRect
    , isInside_ :: Bool
} deriving (Eq, Ord, Show)


{- -- to be maybe used to split renderWalls up sometime?
data RenderWallRoom = RenderWallRoom {
      rRoom :: ! NodeLabel
    , rIsCorridor :: ! Bool
    , rObjects :: [(Coord, Object)]
    , rWalls :: [(Coord, Tile)]
} deriving (Show)

markRoomsAsCorridors :: DynGraph gr => gr NodeLabel EdgeLabel -> gr RenderWallRoom EdgeLabel
markRoomsAsCorridors g = 
    gmapDouble (\(inn, n, l, out) ->
                    (inn, n, l { rIsCorridor = (isInside_ $ rRoom l) && (any isCorridor . map connectivity_ . map snd . lsuc g $ n) }, out))
    . nmap (\r -> RenderWallRoom r False [] []) $ g
-}
{-
Assigning corners as glass or plain wall can be a bit subtle. Marking outside floor as ~, inside floor as ., plain wall as W and glass wall as g, we have:

.G~
W W
...

^ center tile is a corner of three rooms, should be W as otherwise corridors can poke windows into rooms that are supposed to be out of sight.
Note that here the top right room is a corridor room but the bottom room is not.

.G~
W W
.W.

^ same logic, though this time as a four-room intersection

.G~
W G
.W.

^ basically the most complex case, I believe. Here the center tile should be W as again a G would allow vision into restricted areas.


In other words, tile priority order: Plain walls from non-corridor rooms, glass walls, plain walls from corridor rooms
-}

-- first: whether this tile came from a corridor room
-- second: false if plain, true if glass
pickWallType :: [(Bool, Bool)] -> Tile
pickWallType xs 
    | (False, False) `elem` xs = makeWall PlainWall
    | any ((==True) . snd) xs = makeWall GlassWall
    | otherwise = makeWall PlainWall

insertList = cfoldr (\(k, v) -> Map.insert k v)
insertListWith f = cfoldr (\(k, v) -> Map.insertWith f k v)

-- todo: DESTROY IT, ISILDUR! CAST THIS FUNCTION INTO THE FIRE! (at your leisure, once you have figured out how to chop it up well)
renderWalls
  :: (MonadRandom m,
      Graph gr, Functor m) =>
     gr NodeLabel EdgeLabel -> m (Map Node [Coord], [(Coord, Tile)])
renderWalls g =
   (\(doors, walls, doorsAcc) -> (doors, Map.assocs $ Map.map pickWallType $ walls)) `liftM`
   (foldM (\(doors, walls, corridorsAcc) (nr1, nr2, le) ->
               let n1 = justLab g nr1
                   n2 = justLab g nr2
                   fullWall = commonWallTiles (room_ n1) (room_ n2)
                   doorableWall = connectiveWallTiles (room_ n1) (room_ n2)
                   roomIsCorridor nr r = (isInside_ $ nr) && (any isCorridor . map connectivity_ . map snd . lsuc g $ r)
                   haveCorridor = roomIsCorridor n1 nr1 || roomIsCorridor n2 nr2 
  
                   asPlainWall :: [Coord] -> Map Coord [(Bool, Bool)] -> Map Coord [(Bool, Bool)]
                   asPlainWall xs m = insertListWith (++) (map (, [(haveCorridor, False)]) xs) m
                   asGlassWall xs m = insertListWith (++) (map (, [(haveCorridor, True)]) xs) m
                   addWall | (isInside_ n1 && roomIsCorridor n1 nr1 && not (isInside_ n2)) = asGlassWall
                           | (isInside_ n2 && roomIsCorridor n2 nr2 && not (isInside_ n1)) = asGlassWall
                           | otherwise = asPlainWall
                   addConnection width = do 
                     start <- case findMatchingCorridor width doorableWall (Map.findWithDefault [] nr1 corridorsAcc 
                                                                            ++ Map.findWithDefault [] nr2 corridorsAcc) of
                                Nothing -> getRandomR (0, length doorableWall - width)
                                Just x -> return x
                     let newDoors = take width $ drop start doorableWall
                     return (Map.insertWith (++) nr1 newDoors $ Map.insertWith (++) nr2 newDoors $ doors, 
                             addWall (fullWall \\ newDoors) walls, 
                             Map.insertWith (++) nr1 newDoors $ Map.insertWith (++) nr2 newDoors $ corridorsAcc)


               in
               case isInside_ n1 || isInside_ n2 of
                 True -> case connectivity_ le of
                           OneDoor ->
                               addConnection 1
                           Corridor width ->
                               addConnection width
                           None -> return (doors, addWall fullWall walls, corridorsAcc)
                 False -> return (doors, walls, corridorsAcc)) (Map.empty, Map.empty, Map.empty) $ filter (\(e1, e2, _) -> e1 < e2) $ labEdges $ g)

findMatchingCorridor width doorable corridors =
    case findMatchingCorridorStart doorable corridors of
      [] -> Nothing
      xs -> let l = length doorable - width in
            case filter (l >=) $ mapMaybe (\x -> findIndex (==x) doorable) xs of
              [] -> Nothing
              (x:_) -> Just x

findMatchingCorridorStart doorable corridors =
    find' x ++ find' y
        where
          find' f = intersectBy (matching f) doorable corridors

renderRooms =
    concatMap (\nl ->
               if isInside_ nl
               then containedCoords (room_ nl)
               else []) .
    map snd . labNodes

type CompoundType = String
type LevelType = String

{-
  OK, so, quickly again:
  - whoever generates the map chooses a level type
  - the level type provides parameters to the map generator, and a distribution of compound types
  - the compound type provides a distribution of room types
  - the room type provides a distribution of furniture layouts

... but we don't really want to implement the upper levels of that structure yet, so we'll just hardcode in the "Campus" leveltype and its compound types
-}

data RoomType = RoomType {
      roomTypeName :: String
    , roomTypeConnections :: (Int, Int)
    , roomTypeSizeRange :: (Int, Int)
    , roomTypeLayoutAlgorithm :: String
} deriving (Show, Read, Eq, Ord)

$(derive [''RoomType])

normalizeLayoutRotation :: FurnitureLayout -> FurnitureLayout
normalizeLayoutRotation l
    | let (Coord xb yb, Coord xe ye) = bounds $ flMap l in (xe - xb) >= (ye - yb)
              = l
    | otherwise = (!!4) $ layoutRotations l


--indexLayoutsByXSizeInto :: [FurnitureLayout] -> Map Int [FurnitureLayout] -> Map Int [FurnitureLayout]
indexLayoutsByXSizeInto =
    cfoldr (\l -> Map.insertWith (flip (++)) ((\(Coord ax _, Coord bx _) -> bx - ax + 1) $ bounds $ flMap $ snd l) [l])


indexLayouts :: [FurnitureCharacters]
             -> [StoredFurnitureLayout] 
             -> Map String FurniturePrototype 
             -> Map String ([(Int, [(Double, FurnitureLayout)])])
indexLayouts characters layouts prototypes = 
    Map.map (Map.toAscList) $ 
    foldr (\(k, l) ->
               Map.alter (\x -> Just $
                          case x of
                            Nothing -> Map.singleton ((\(Coord ax _, Coord bx _) -> bx - ax + 1) $ bounds $ flMap $ snd l) [l]
                            Just e -> indexLayoutsByXSizeInto [l] e)
               k)
    Map.empty $
    concatMap (\sl -> let rl = second normalizeLayoutRotation $ readFurnitureLayout characters sl prototypes in
                      [(k, rl) | k <- layoutRooms sl]) $
    layouts

type RoomDecorator m = (Array Coord Tile -> BoundsRect -> [Coord] -> m [(Object, Coord)])

assignOfficeCompoundRoomTypes :: 
    (Monad m, MonadRandom m, Functor m)
    => [RoomType]
        -> [(BoundsRect, [Coord])]
        -> m [String]
assignOfficeCompoundRoomTypes types rooms = mapM (\room -> return "officeGeneric") rooms

-- todo: Once there's more than one room type to actually render, throw this at it
assignRoomTypes :: 
    (Graph gr, Monad m, MonadRandom m, Functor m)
    => [RoomType]
        -> gr NodeLabel EdgeLabel
        -> m [(Int, String)]
assignRoomTypes types compound =
    let rooms = map (Arrow.second room_) $ labNodes compound
    in return $ map (Arrow.second (const "officeGeneric")) $ rooms

renderWorld
  :: (MonadIO m, Functor m, MonadRandom m, Graph gr) =>
     gr NodeLabel EdgeLabel
  -> Array Coord Tile
  -> [gr NodeLabel EdgeLabel]
  -> m ([(Object, Coord)], Array Coord Tile, [[IdlingPoint]])
renderWorld g a compounds = do
  mlayouts <- liftIO (loadDataFromFile "FurnitureLayouts.yaml" :: IO (Either String [StoredFurnitureLayout]))
  mcharacters <- liftIO (loadDataFromFile "FurnitureCharacters.yaml" :: IO (Either String [FurnitureCharacters]))
  mrooms <- liftIO (loadDataFromFile "Rooms.yaml" :: IO (Either String [RoomType]))
  mprototypes <- liftIO (loadDataFromFile "FurniturePrototypes.yaml" :: IO (Either String [FurniturePrototype]))

  case (mlayouts, mcharacters, mrooms, mprototypes) of
    (Right layouts, Right characters, Right rooms, Right prototypes) -> do
         let layoutsIndexed = undefined -- indexLayouts characters layouts $ indexPrototypes prototypes
             roomsCompounds :: Map Int Int -- mapping from room number to compound number
             roomsCompounds = Map.fromList $ concat $ zipWith (\compound num -> map (,num) $ nodes compound) compounds [1..]
         (doorsMap, walls) <- renderWalls g
         let floors = renderRooms g
             doors = nub $ concat $ Map.elems doorsMap
             walledWorld = a // (map (, makeFloor Concrete) (floors ++ doors))  // walls
         roomTypes <- Map.fromList `fmap` concat `fmap` mapM (assignRoomTypes rooms) compounds
         (roomDecor, idlingPoints) <- mapAndUnzipM (\(nr, r) -> 
                          decorateRoom layoutsIndexed (roomTypes Map.! nr) walledWorld (room_ r) (doorsMap Map.! nr)) $ filter (isInside_ . snd) $ labNodes g
         let allObjects = map (door False,) doors ++ concat roomDecor
         return (allObjects, walledWorld, idlingPoints)
    _ -> fail ("renderWorld: Couldn't parse some YAML data file: " ++
               (intercalate "\n" $ catMaybes [("in FurnitureLayouts.yaml: " ++) `fmap` show `fmap` getLeft mlayouts, 
                                              ("in FurnitureCharacters.yaml: " ++) `fmap` show `fmap` getLeft mcharacters, 
                                              ("in Rooms.yaml: " ++) `fmap` show `fmap` getLeft mrooms, 
                                              ("in FurniturePrototypes.yaml: " ++) `fmap` show `fmap` getLeft mprototypes]))

getLeft :: Either a b -> Maybe a
getLeft (Left a) = Just a
getLeft _ = Nothing

$( deriveAccessors ''EdgeLabel )
$( deriveAccessors ''NodeLabel )

isConnector None = False
isConnector _ = True

saneLabEdges = map (\(x, y, z) -> ((x, y), z)) . labEdges

-- technically this could convert to a different graph type, I'm fixing the type though because that only confuses matters
componentsOf :: (Graph gr, DynGraph gr) => gr a b -> [gr a b]
componentsOf g =
    map (\nodes -> buildGr $ gsel ((`elem` nodes) . node') g) $ components g

calcCompounds g = 
    componentsOf $ 
    delEdges (map fst . filter (not . isConnector . connectivity_ . snd) $ saneLabEdges g) $ 
    delNodes (map fst . filter (not . isInside_ . snd) $ labNodes g) g

connectCompound g compound =
    foldr (adjustPathU id (\l -> case connectivity_ l of
                                   Corridor _ -> l
                                   _ -> (connectivity ^= OneDoor) $ l) . map fst . (\(LP x) -> x)) g .
    msTree . 
    lemap (\(from, to, l) -> 
               case connectivity_ l of
                 Corridor _ -> 0
                 OneDoor -> 1 -- never occurs when initially connecting compounds, but useful for less door spam with connectUp (?)
                 _ -> let outside = (isInside_ $ justLab g from) /= (isInside_ $ justLab g to)
                          fromR = room_ $ justLab g from
                          toR = room_ $ justLab g to
                          corridorishness = 1
--                          corridorishness = maximum [fst $ snd fromR, snd $ snd fromR, fst $ snd toR, snd $ snd toR]
                      in ((100000 `div` corridorishness) * (if outside then 10 else 2))) $ compound

addRandomConnections (CampusArgs {randomConnectionChance, randomOutsideConnectionChance}) g = do
  seed <- getRandom
  return $ lemap (\(from, to, l) -> 
                      let fromR = justLab g from
                          toR = justLab g to 
                          roll = randomEdgeValue from to seed in
                      case (connectivity_ l, isInside_ fromR /= isInside_ toR, roll < randomConnectionChance, roll < randomOutsideConnectionChance) of
                        (None, False, True, _) -> (connectivity ^= OneDoor) l
                        (None, True, _, True) -> (connectivity ^= OneDoor) l
                        _ -> l) g

-- fgl does not provide monadic graph manipulation functions so I use node IDs plus a seed as a source of pseudorandom numbers
randomEdgeValue from to seed = fst $ random $ mkStdGen (seed + 65536 * from + to)

connectUp g = do 
  return $ connectCompound g g

adjustNodeLabel :: (DynGraph gr, Graph gr) => (a -> a) -> Node -> gr a b -> gr a b
adjustNodeLabel f n g = 
    let (mc, ng) = match n g in
    case mc of
      Nothing -> g
      Just (into, n, l, outOf) -> (into, n, f l, outOf) & ng


-- adjust the nodes and edges across a list of nodes on an undirected graph
adjustPathU :: (DynGraph gr) => (a -> a) -> (b -> b) -> [Node] -> gr a b -> gr a b
adjustPathU fn fe ns g =
    cfoldr (\(nFrom, nTo) g -> 
                case match nTo g of
                  (Nothing, _) -> g
                  (Just (links, n, l, _), newG) ->
                      let newLinks = adjustEdgeLabel fe nFrom links in
                      (newLinks, n, fn l, newLinks) & newG) (Control.Monad.ap zip tail $ ns)
    $ case ns of
        [] -> g
        (x:_) -> adjustNodeLabel fn x g -- also handle the first node in the path

adjustEdgeLabel :: (a -> a) -> Int -> [(a, Int)] -> [(a, Int)]
adjustEdgeLabel f nSearch [] = []
adjustEdgeLabel f nSearch (x@(l, n):xs)
    | n == nSearch = (f l, n) : xs
    | otherwise = x : adjustEdgeLabel f nSearch xs

lnmap :: DynGraph gr => (LNode a -> c) -> gr a b -> gr c b
lnmap f = gmap (\(inn, node, label, out) -> (inn, node, f (node, label), out))

lemap :: DynGraph gr => (LEdge b -> c) -> gr a b -> gr a c
lemap f = gmap (\(to, node, label, from) -> (mapTo node f to, node, label, mapFrom node f from))
          where
            mapTo to g = map (\(label, from) -> (g (from, to, label), from))
            mapFrom from g = map (\(label, to) -> (g (from, to, label), to))
