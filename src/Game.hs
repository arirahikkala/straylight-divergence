{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, BangPatterns, NamedFieldPuns #-}

module Game where

import Data.Array ((!), bounds, inRange, Array)
import Data.Maybe (mapMaybe, maybeToList, fromMaybe)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Bits

import Data.Accessor

import Control.Monad

import BasicTypes
import Object
import CommonTypes
import Compound
import Tile

import qualified LocationMap as Loc

import Util (maybeHead, lookupAssocSet, squaredEuclideanDistance, chebyshevDistance, none, (!/))

squaredVisionDistanceLimit = 7^2

adjacentsInWorldBounds level c = do
  bs <- bounds `fmap` gsLevel level world_
  return $ adjacentsBounded bs c

getLocation ro =
    do p <- gsGlobal positions_
       return $ Loc.lookupLoc (ref ro) p

getPosObjs c =
    do p <- gsGlobal positions_
       let rs = Set.elems $ Loc.lookupObjRefs c p
       os <- mapM dereferObj rs
       return $ zipWith SpecificObject rs os
--runGameT st (GameT a) = runStateT a st

getManyPosObjs cs =
    do p <- gsGlobal positions_
       let rs = concat $ map Set.elems $ map (flip Loc.lookupObjRefs p) cs
       os <- mapM dereferObj rs
       return $ zipWith SpecificObject rs os

mapPosition r =
    do p <- gsGlobal positions_
       let loc = Loc.lookupLoc r p
       return $ go p loc
    where
      go _ Nothing = error "tried to look for mapPosition of inexistent object"
      go _ (Just (OnMap l c)) = (l, c)
      go p (Just (InContainer r)) = go p (Loc.lookupLoc r p)

-- | Does the tile whose position is given in the last argument block vision 
-- | (i.e. may be visible itself, but disallow vision behind it)? 
{-
tileBlocksVision :: MonadGame m => 
                    World -- ^ we block if the world contains a wall here
                 -> Positions -- ^ to look for visibility-blocking objects
                 -> Coord -- ^ to see how far we're from the viewer
                 -> Coord -- ^ the tile to check 
                 -> m Bool
-}

-- xxx: should carried objects be able to block vision?
posBlocksVision world references positions charPos c =
  let os = map (references Map.!) . Set.elems $ Loc.lookupObjRefs c positions in
  not (inRange (bounds world) (coord c))
  || (squaredEuclideanDistance charPos (coord c) > squaredVisionDistanceLimit) 
  || (maybe True tileBlocksVision (world !/ (coord c))) 
         || (any objBlocksVision os)

-- | Are these Coords in each other's Moore neighborhood?
adjacent :: Coord -> Coord -> Bool
adjacent a b = chebyshevDistance a b == 1

-- | Are these Coords the same or in each other's Moore neighborhood?
adjacentOrSame :: Coord -> Coord -> Bool
adjacentOrSame a b = chebyshevDistance a b <= 1



-- | Find an object in the references map given its identity.
dereferObj objref = 
    do refs <- gsGlobal references_
       case Map.lookup objref refs of
         Nothing -> fail "attempted to derefer inexistent ObjRef"
         Just x -> return x

dereferObjToSpecific objref = 
    do o <- dereferObj objref
       return $ SpecificObject objref o

--allContents :: MonadState GameState m => ObjRef -> m [ObjRef]
{-
allContents r = 
    do o <- dereferObj r
       case (mContents o) of
         Nothing -> return []
         Just xs -> let e = Set.elems xs in
                    do rs <- mapM allContents e
                       return (r : concat rs)
-}

-- | Update an object in the game state once you've finished working it out
--touch :: MonadState GameState m => Object -> m ()
touch r !o =
    do mGlobal (references ^: Map.insert r o)

isVisibleToPlayer r =
    do pos <- mapPosition r
       playerLevel <- fst `fmap` mapPosition playerCharRef
       v <- gsLevel playerLevel playerVisible_
       return $ ((playerLevel == fst pos) && (Set.member (snd pos) v))

tileEnterable c =
    do w <- gsLevel (fst c) world_
       p <- gsGlobal positions_
       os <- mapM dereferObj (Set.elems $ Loc.lookupObjRefs (OnMap (fst c) (snd c)) p)
       return (not (maybe True tileBlocksMovement (w !/ snd c)) && none objBlocksWalking os)

tileEnterableP :: Array Coord Tile -> Loc.LocationMap ObjRef Location -> References -> (LevelRef, Coord) -> Bool
tileEnterableP w p refs c =
    not (maybe True tileBlocksMovement (w !/ snd c)) && 
    (none objBlocksWalking . mapMaybe (\r -> Map.lookup r refs) . Set.elems $ 
     Loc.lookupObjRefs (uncurry OnMap $ c) p)

tryWalkTo s@(SpecificObject r o) c =
    do e <- tileEnterable c
       oldC <- mapPosition r
       if e && adjacentOrSame (snd c) (snd oldC)
          then moveObject s (OnMap (fst c) (snd c)) >> return Nothing
          else return $ Just "can't walk there" -- xxx: better message

playerCharRef = ObjRef 1

--moveObject :: MonadState GameState m => Object -> Location -> m ()
moveObject s@(SpecificObject r o) targLoc = do
  mGlobal (positions ^: Loc.move r targLoc)

supplyId = do
  i <- gsGlobal firstUnusedId_
  mGlobal $ (firstUnusedId ^: succ)
  return i

supplyIds n =
    do i@(ObjRef ri) <- gsGlobal firstUnusedId_
       mGlobal $ (firstUnusedId ^= ObjRef (n + ri))
       return $ take n $ iterate succ i


passTurn = mGlobal (turnDone ^= True)

playerLevel = fst `fmap` mapPosition playerCharRef

gPlayerLevel = do
  level <- fst `fmap` mapPosition playerCharRef
  gLevel level

gsPlayerLevel f = do
  level <- fst `fmap` mapPosition playerCharRef
  gsLevel level f

mPlayerLevel f = do
  level <- fst `fmap` mapPosition playerCharRef
  mLevel level f

pPlayerLevel x = do
  level <- fst `fmap` mapPosition playerCharRef
  pLevel level x

toSpecificObject :: Maybe Object -> ObjRef -> SpecificObject
toSpecificObject Nothing r = error ("toSpecificObject: Tried to lookup ObjRef " ++ show r)
toSpecificObject (Just o) r = SpecificObject r o

--lookupObjs :: Monad m => Location -> LocationMap ObjRef Location -> GameT m (Set ObjRef)
lookupObjs r ml l =
    Set.mapMonotonic (\ref -> toSpecificObject (Map.lookup ref r) ref) $ Loc.lookupObjRefs l ml

adjustPos N (Coord x y) = Coord x (y-1)
adjustPos NW (Coord x y) = Coord (x-1) (y-1)
adjustPos W (Coord x y) = Coord (x-1) y
adjustPos SW (Coord x y) = Coord (x-1) (y+1)
adjustPos S (Coord x y) = Coord x (y+1)
adjustPos SE (Coord x y) = Coord (x+1) (y+1)
adjustPos E (Coord x y) = Coord (x+1) y
adjustPos NE (Coord x y) = Coord (x+1) (y-1)

adjacents :: Coord -> [Coord]
adjacents a = map (flip adjustPos a) [N, S, E, W, NE, SE, NW, SW]

adjacentsBounded :: (Coord, Coord) -> Coord -> [Coord]
adjacentsBounded bs = filter (inRange bs) . adjacents

getCompoundRooms :: (Monad m, Functor m) => LevelRef -> CompoundRef -> GameT m [BoundsRect]
getCompoundRooms l c = do
  (Compounds {compoundsRefs_}) <- gsLevel l compounds_
  return . concatMap compoundRooms_ . maybeToList $ Map.lookup c compoundsRefs_


lookupCompound :: (Monad m, Functor m) => LevelRef -> Coord -> GameT m CompoundRef
lookupCompound l c = do
  (Compounds {compoundsCoords_}) <- gsLevel l compounds_
-- outside of the map is considered to be in the "outside" compound
  return $ fromMaybe (CompoundRef 0) (compoundsCoords_ !/ c)

-- outside not counted
numCompounds :: (Functor m, Monad m) => LevelRef -> GameT m Int
numCompounds l = do
  (Compounds {compoundsRefs_}) <- gsLevel l compounds_
  return $ Map.size compoundsRefs_