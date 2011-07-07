{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, TupleSections #-}
module AI (act) where

import Util (uniformRandomPick, renderCoordCharArray)
import Rect (rectCoords)
import Compound
import Game
import BasicTypes
import CommonTypes
import Object
import Control.Monad.Maybe
import Control.Monad.Random.Class (getRandomR)
import Data.List (nub, sort)
import Data.Array.IArray ((//), bounds, amap, listArray, elems, range)
import Data.Array.Unboxed

import Control.Monad.State

import AStarFFI

import Data.Accessor

act r =
    do o <- dereferObj r
       pos <- mapPosition r
    -- todo: add a stay-still fallback for when there's nowhere to go
       let plan = movementPlan_ . ai_ $ o
       case plan of
         [] -> do p <- newMovementPlan pos
                  touch r ((movementPlan <. ai ^= p) $ o)
         _ -> return ()
       plan <- (movementPlan_ . ai_) `fmap` dereferObj r
       case plan of
         [] -> return ()
         (x:xs) -> do touch r ((movementPlan <. ai ^= xs) $ o)
                      walkRes <- tryWalkTo (SpecificObject r o) (fst pos, x)
                      case walkRes of
                        Nothing -> return ()
                        Just _ -> do touch r ((movementPlan <. ai ^= []) $ o)
                                     act r



newMovementPlan (level, c) =
    do goal <- randomDistantTargetCoord level c
       w <- gsLevel level world_
       enterable <- mapM tileEnterable $ map (level, ) $ range $ bounds w

       let plan = drop 1 $ astar (listArray (bounds w) enterable // [(c, True)]) c goal
       return plan

-- Distant = not adjacent or same as the given coordinate
-- also now restricted to the compound you're currently in!
randomDistantTargetCoord level c = do
  compound' <- lookupCompound level c
  compound <- case compound' of
                CompoundRef 0 -> do 
                               sum <- numCompounds level
                               num <- getRandomR (1, sum)
                               return (CompoundRef num)
                _ -> return compound'
  rooms <- getCompoundRooms level compound
  mroom <- uniformRandomPick rooms
  case mroom of
    Nothing -> fail "randomDistantTargetCoord had nowhere to go, no fallback yet"
    Just (room, _) -> do 
               coord <- uniformRandomPick $ rectCoords $ room
               case coord of
                 Nothing -> fail "randomDistantTargetCoord had nowhere to go, no fallback yet"
                 Just (coord, _) -> return coord

{-
  (Coord xBound yBound) <- snd `fmap` bounds `fmap` gsLevel level world_
  x <- getRandomR (0, xBound)
  y <- getRandomR (0, yBound)
  let cand = Coord x y
  e <- tileEnterable (level, cand)
  if e && not (adjacentOrSame c cand) 
     then return cand
     else randomDistantTargetCoord level c


-}