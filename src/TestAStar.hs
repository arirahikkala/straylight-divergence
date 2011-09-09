module TestAStar where

import BasicTypes
import AStarFFI
import Util (lists2dToArray, array2dToLists)
import Data.Array.Unboxed

testLevel = 
    [".....",
     "***..",
     ".....",
     ".***.",
     "....."]

testArray :: UArray Coord Bool
testArray = lists2dToArray $ map (map (=='.')) $ testLevel

test = astar testArray (Coord 0 0) (Coord 3 4)

renderPathA :: Array Coord Char
renderPathA = lists2dToArray testLevel // zip test (cycle ['0'..'9'])

showPath = mapM_ putStrLn $ array2dToLists renderPathA