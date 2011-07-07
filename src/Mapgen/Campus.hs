{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving, ExistentialQuantification, TemplateHaskell, TupleSections, ParallelListComp, NamedFieldPuns, NoMonomorphismRestriction, ScopedTypeVariables #-}

--module Mapgen.Campus (makeMap, CampusArgs (..)) where
module Mapgen.Campus where

import Object
import BasicTypes
import CommonTypes
import Mapgen.Rect
import Mapgen.Furniture

import Control.Monad.Random
import Control.Monad.Trans (MonadIO, liftIO)

import Control.Arrow

import Data.Array.IArray

import Data.Bits

import Data.List (group, sort, delete, (\\), nubBy)
import Data.Maybe (catMaybes)

import Data.VectorSpace (magnitude, (^-^), (^+^))

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Graph.Inductive (LPath (..), Gr, sp, gmap, Graph, DynGraph, Node, LEdge, lab, match, (&), labNodes, labEdges, emap, nmap, delNodes, lsuc, lpre, neighbors, noNodes)
import Data.Graph.Inductive.Basic (gsel)
import Data.Graph.Inductive.Query (msTree, bft)

import Data.Accessor
import Data.Accessor.Template

import Control.Monad (ap, foldM, replicateM, liftM2)

import Util (cfoldr, uniformRandomPick, justUniformRandomPick, takeWhileAndOne, matching, ntimesM, none, lengthAtLeast)
import Mapgen.Util (showCoordIntArray, showCoordIntArrayN, floorEverything, addEdgeWalls, justLab)
import Mapgen.RectTree

import Tile

import Mapgen.Room
import Mapgen.CampusArgs

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

--addOutside _ _ g = addOutsideGently g

addOutside (CampusArgs {numOutsidePaths}) a g = --addOutsideGently g
  let roomGraph = nmap room_ g
      ws = walls $ ((^+^ Coord 1 1) *** (^-^ Coord 2 2)) $ bounds a
      wallLengths = map length ws in do
  wallChoices <- replicateM numOutsidePaths (do from <- getRandomR (0, 3)
                                                to <- getRandomR (0, 2)
                                                return (from, if to >= from then to + 1 else to))
  positions <- mapM (\(fromWall, toWall) -> 
                         do fromIndex <- getRandomR (0, wallLengths !! fromWall - 1)
                            toIndex <- getRandomR (0, wallLengths !! toWall - 1)
                            return (ws !! fromWall !! fromIndex, ws !! toWall !! toIndex)) wallChoices
  return $ foldr (\(begin, end) -> 
                      adjustPathU (isInside ^= False) id (path roomGraph (a ! begin) (a ! end) 3)) g positions


-- ^ Break up big compounds of insideness. Currently works by picking a random node whose neighbors' neighbors are all inside, and connecting that node by the shortest path in terms of number of nodes to an outside node.

touchUpOutside g =
    let roomGraph = nmap room_ g
        choices = map fst $ filter (\(n, l) -> isInside_ l && all (isInside_ . snd) (concatMap (lneighbors g . fst) $ lneighbors g n)) $ labNodes g
        nodeIsOutsideRoom n = case lab g n of
                               Just (RoomLabel { isInside_ = False } ) -> True
                               _ -> False
    in do mStart <- uniformRandomPick choices
          case mStart of
            Nothing -> return g
            Just (from, _) -> 
                -- todo: handle connection width constraints? (ref. path's minWidth argument and paring-out of edges thinner than the constraint)
                case filter (nodeIsOutsideRoom . head) $ bft from g of
                  [] -> return g
                  (xs:_) -> touchUpOutside $ adjustPathU (isInside ^= False) id xs g

lneighbors g n = 
    let ns = neighbors g n in 
    catMaybes $ zipWith (\a b -> liftM2 (,) (Just a) b) ns (map (lab g) ns)

addCrossingCorridors (CampusArgs {numCrossingCorridors}) a g = do
  let innerBounds = ((^+^ Coord 1 1) *** (^-^ Coord 2 2)) $ bounds a
      roomGraph = nmap room_ g
  positions <- replicateM numCrossingCorridors (liftM2 (,) (getRandomR innerBounds) (getRandomR innerBounds))
  return $ foldr (\(begin, end) -> adjustPathU (isInside ^= True) (connectivity ^= Corridor 2) (path roomGraph (a ! begin) (a ! end) 2)) g positions

addCorridors (CampusArgs {numCorridors}) g = do
  let roomGraph = nmap room_ g
      insideNodes = map fst . filter (isInside_ . snd) $ labNodes g
      outsideNodes = map fst . filter (not . isInside_ . snd) $ labNodes g
  if not (lengthAtLeast 1 insideNodes && lengthAtLeast 1 outsideNodes)
     then return g
     else do positions <- replicateM numCorridors (liftM2 (,) (fst `fmap` justUniformRandomPick insideNodes) (fst `fmap` justUniformRandomPick outsideNodes))
             return $ foldr (\(begin, end) -> 
                                 let p = takeWhileAndOne (\n -> maybe False isInside_ (lab g n)) $ path roomGraph begin end 2 in
                                 adjustPathU id (connectivity ^= Corridor 2) p) g positions



makeMap
  :: (Functor m, MonadRandom m, MonadIO m) =>
     CampusArgs -> (Coord, Coord) -> m (World, [(Object, Coord)], [[BoundsRect]])
makeMap args mapBounds@(boundsMin, boundsMax) = do
--  layoutCharacters <- liftIO $ justReadJsonFile "LayoutCharacters.json"
-- roomMap can't deal with non-zero-indexed arrays, but we need space to put the top and left edge walls, so move things about a bit
-- note that boundsMax only needs to be made smaller to make space for walls on the top and left; the bottom and right produce
-- empty space naturally since those tiles are owned by rooms in the roomgraph (but don't have walls rendered on them because there 
-- are no rooms on the other side to have edges with)
  (roomArray, roomGraph) <- (id *** nmap ((^+^ Coord 1 1) *** id)) `fmap` roomMap mapBounds defaultRectTreeArgs
  let world1 = floorEverything $ accumArray const 0 mapBounds []
  n <- getRandomR (0, noNodes roomGraph - 1)
--  roomGraph2 <- addOutside args roomArray (nmap ((flip RoomLabel) True) $ roomGraph)
  let roomGraph2 = adjustNodeLabel (isInside ^= False) n $ nmap ((flip RoomLabel) True) $ roomGraph
--  roomGraph3 <- addCrossingCorridors args roomArray (emap (const $ EdgeLabel None) roomGraph2)
  let roomGraph3 = emap (const $ EdgeLabel None) roomGraph2
  roomGraph4 <- touchUpOutside roomGraph3
  roomGraph5 <- addCorridors args roomGraph4
  roomGraph6 <- connectUp roomGraph5
  let normalCompounds = calcCompounds roomGraph6
  roomGraph7 <- addRandomConnections args roomGraph6
  (objs, (world3 :: Array Coord Tile)) <- renderWorld roomGraph7 world1 normalCompounds
  return $ (addEdgeWalls (bounds world3) world3, objs, map (map (room_ . snd) . labNodes) normalCompounds)



isConnection None = False
isConnection (Corridor {}) = True
isConnection (OneDoor {}) = True

numConnections (to, _, r, from) = length . filter isConnection . map connectivity_ . map fst $ nubBy (matching snd) $ (to ++ from)

addFeature i f = setBit i (fromEnum f)

umap f = map f . gsel (const True)


roomConnectivityStats g =
    map (head &&& length) . group . sort . map snd .
    filter (isInside_ . justLab g . fst) .
    Map.toList $
    foldr (\(from, _, e) g -> 
               if isConnection . connectivity_ $ e
               then Map.insertWith (+) from 1 g
               else g) Map.empty $ labEdges g


path roomGraph start end minWidth = 
    sp start end distanceGraph
    where
      distanceGraph = gmapU (\(edges, rNum, r) -> 
                                let edgesWithDistance = map (\(_, n) -> (magnitude (approximateCenter r ^-^ (approximateCenter $ justLab roomGraph n)), n)) $
                                                        filter (\(_, n) -> minWidth <= connectionWidth r (justLab roomGraph n)) $ edges in
                                (edgesWithDistance, rNum, ())) roomGraph

--gmapU :: Graph a b => ((Adj b, Node, a) -> (Adj c, Node, d)) -> gr a b -> gr c d
gmapU f = gmap (\(into, n, l, _) -> (\(links, n, l) -> (links, n, l, links)) $ f (into, n, l))



