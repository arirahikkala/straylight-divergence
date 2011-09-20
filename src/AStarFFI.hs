{-# LANGUAGE ForeignFunctionInterface #-}
module AStarFFI (astar, astarIO) where

import Foreign
import Foreign.C

import Data.Array.IArray
import Data.Array.Unboxed
import BasicTypes
import Control.Monad.Trans (liftIO) 
import Data.VectorSpace
import Data.List (transpose)
import Util (array2dToLists)
import Control.Monad

import System.CPUTime

type AStarC = Ptr CChar -> 
    Ptr CInt ->
    CInt -> CInt ->
    CInt -> 
    CInt ->
    IO (Ptr CInt)


foreign import ccall unsafe "AStar.h astar_compute" astarC :: 
    AStarC

foreign import ccall unsafe "AStar.h astar_unopt_compute" astarUnoptC :: 
    AStarC

foreign import ccall unsafe "stdlib.h free" freeC :: Ptr a -> IO ()

interpolatePath :: [Coord] -> [Coord]
interpolatePath [] = []
interpolatePath xs = concat $ zipWith ((drop 1 .) . interpolateSpan) xs (tail xs)

interpolateSpan :: Coord -> Coord -> [Coord]
interpolateSpan from@(Coord fX fY) to@(Coord tX tY) 
    | from == to = [from]
    | otherwise = zipWith Coord (interpolate fX tX) (interpolate fY tY)

-- an enumFromTo-ish function with appropriate behaviour for interpolateSpan
interpolate :: Integral a => a -> a -> [a]
interpolate from to | from < to = enumFromTo from to
                    | from > to = enumFromThenTo from (pred from) to
                    | from == to = repeat from

astarIO :: UArray Coord Bool -> Coord -> Coord -> IO (Maybe [Coord])
astarIO hA (Coord hStartX hStartY) (Coord hGoalX hGoalY) = do
  let (Coord hBoundX' hBoundY') = snd (bounds hA) ^-^ fst (bounds hA)
      hBoundX = hBoundX' + 1
      hBoundY = hBoundY' + 1
      (begin@(Coord beginX beginY), Coord hEndX hEndY) = bounds hA
      toC = (fromIntegral :: Int -> CInt)
      toH = (fromIntegral :: CInt -> Int)
  
  alloca $ \solLength -> do
    a <- newArray (map (\n -> if n then 1 else 0) $ 
                   concat $ array2dToLists $ hA)

    s <- astarC a 
         solLength 
         (toC hBoundX)
         (toC hBoundY)
         (toC $ getIndex hBoundX (hStartX - beginX) (hStartY - beginY))
         (toC $ getIndex hBoundX (hGoalX - beginX) (hGoalY - beginY))

    rvLen <- peek solLength
    rv <- if rvLen > 0 
             then map (getCoord hBoundX) `fmap` map toH `fmap` peekArrayRev (toH rvLen) s
             else return []
    free s
    free a
    return $ case rv of 
               [] -> Nothing
               xs -> Just $ interpolatePath $ map (^+^ begin) $ xs

peekArrayRev :: Storable a => Int -> Ptr a -> IO [a]
peekArrayRev size ptr 
    | size <= 0 = return []
    | otherwise = f 0 []
  where
    f n acc | n == size + 1 = return acc
            | otherwise = do e <- peekElemOff ptr n; f (n+1) (e:acc)

getIndex bX x y = y * bX + x
getCoord bX i = Coord (i `mod` bX) (i `div` bX)

astar hA hB hE 
    | hB == hE = Just []
    | otherwise = unsafePerformIO (astarIO hA hB hE)
