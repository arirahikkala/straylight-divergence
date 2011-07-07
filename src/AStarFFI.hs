{-# LANGUAGE ForeignFunctionInterface #-}
module AStarFFI (astar, astarIO) where

import Foreign
import Foreign.C

import Data.Array.IArray
import Data.Array.Unboxed
import BasicTypes
import Control.Monad.Trans (liftIO) 

foreign import ccall unsafe "AStar.h astar" astarC :: 
    Ptr CInt -> 
    Ptr CInt ->
    CInt -> CInt ->
    CInt -> 
    CInt ->
    IO (Ptr CInt)

foreign import ccall unsafe "stdlib.h free" freeC :: Ptr a -> IO ()

astarIO :: UArray Coord Bool -> Coord -> Coord -> IO [Coord]
astarIO hA (Coord hBeginX hBeginY) (Coord hEndX hEndY) = do
  let (_, (Coord hBoundX' hBoundY')) = bounds hA
      hBoundX = hBoundX' + 1
      hBoundY = hBoundY' + 1
      toC = (fromIntegral :: Int -> CInt)
      toH = (fromIntegral :: CInt -> Int)
  alloca $ \solLength -> do
    a <- newArray (map (\n -> if n then 1 else -1) $ 
                   elems $ 
                   ixmap (Coord 0 0, Coord hBoundY' hBoundX') (\(Coord x y) -> Coord y x) hA)

    s <- astarC a 
         solLength 
         (toC hBoundX) 
         (toC hBoundY) 
         (toC $ getIndex hBoundX hBeginX hBeginY)
         (toC $ getIndex hBoundX hEndX hEndY)
    rvLen <- peek solLength
    rv <- if rvLen > 0 
             then map (getCoord hBoundX) `fmap` map toH `fmap` peekArrayRev (toH rvLen) s
             else return []
    free s
    free a
    return rv

peekArrayRev :: Storable a => Int -> Ptr a -> IO [a]
peekArrayRev size ptr 
    | size <= 0 = return []
    | otherwise = f 0 []
  where
    f n acc | n == size = return acc
            | otherwise = do e <- peekElemOff ptr n; f (n+1) (e:acc)

getIndex bX x y = y * bX + x
getCoord bX i = Coord (i `mod` bX) (i `div` bX)

astar hA hB hE = unsafePerformIO (astarIO hA hB hE)
