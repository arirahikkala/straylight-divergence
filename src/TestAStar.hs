{-# LANGUAGE FlexibleInstances, TupleSections #-}
module TestAStar where

import BasicTypes
import AStarFFI
import Util (lists2dToArray, array2dToLists)
import Data.Array.Unboxed
import Test.QuickCheck
import qualified Data.Graph.AStar as Cale
import qualified Data.Set as Set
import Control.Monad
import Data.List (foldl')
import Data.VectorSpace

scenarioFiles = words $ "bg512/AR0300SR.map.scen"

data Scenario = Scenario {
      mapFile :: String
    , start :: Coord
    , goal :: Coord } deriving Show

--readScenarioFile :: String -> IO [Scenario]
readScenarioFile s = do
  file <- readFile ("benchmarkProblems/scenarios/" ++ s)
  return $ map ((\(_:b:_:_:e:f:g:h:i:[]) -> Scenario b (Coord (read e) (read f)) (Coord (read g) (read h))) . words) $ drop 1 $  lines file

readMapFile :: String -> IO (UArray Coord Bool)
readMapFile s = do
  file <- readFile ("benchmarkProblems/" ++ s)
  let ls = lines file
      lsw = map words $ ls
      height = lsw !! 1 !! 1
      width = lsw !! 2 !! 1
  return $ lists2dToArray $ (map . map) (\c -> if c `elem` ".G" then True else False) $ drop 4 ls

pathLength xs = foldl' (\a (from, to) -> a + magnitude (from ^-^ to)) 0 $ zip xs (tail xs)

differenceIsMinimal Nothing Nothing = True
differenceIsMinimal (Just a) (Just b) = abs (a - b) < 0.001
differenceIsMinimal _ _  = False
    

doValidity = do
  scens <- mapM readScenarioFile scenarioFiles
  maps <- mapM (readMapFile . mapFile . head) scens
  mapM_ (\(m, i, s) -> 
             do opt <- astarIO True m (start s) (goal s)
                unopt <- astarIO False m (start s) (goal s)
                unless (differenceIsMinimal (fmap pathLength opt) (fmap pathLength unopt))
                         $ print (show s ++ 
                                  ": opt says " ++ show opt ++ 
                                  " but unopt says " ++ show unopt))
        $ take 5 $ concat $ zipWith3 (\m s i -> map (m, i,) s) maps scens [length scens, length scens - 1..]

doBenchmark = do
  scens <- mapM readScenarioFile scenarioFiles
  maps <- mapM (readMapFile . mapFile . head) scens
  (opt, unopt) <-
         foldM (\(accA, accB) (m, s) -> do 
           opt <- astarBenchmarkIO True m (start s) (goal s)
           unopt <- astarBenchmarkIO False m (start s) (goal s)
           putStrLn (show s ++ ": " ++ show (fromIntegral opt / fromIntegral unopt))
           return (accA + opt, accB + unopt)) (0, 0)
        $ concat $ zipWith (\m s -> map (m, ) s) maps scens
  print ("opt/unopt seconds: " ++ show (fromIntegral opt/fromIntegral unopt))

main = doBenchmark

testLevel = ["###################################################",
             "#   #     #    #                  #    #          #",
             "#   #     #                       #    #          #",
             "#   #     #    #                  #    #          #",
             "#              #                  #               #",
             "# ##### ##### ##                       #          #",
             "#    #    #                       # ####          #",
             "#         #    ## ### #############               #",
             "#    #             #                              #",
             "#    #    #    #   #                              #",
             "## ##### #### ##   #                              #",
             "#    #             #                              #",
             "#              #   #                              #",
             "#    #         #   #                   ######### ##",
             "# ########### ### ######           ## ##          #",
             "#    #         #       #           #          #   #",
             "#    #         #       #           #   #      #   #",
             "#    #         #       #           #   #      #   #",
             "#    #         #       #           #   #      #   #",
             "#    #         #       #           ## ##      #   #",
             "#### #         #       #   #########          #   #",
             "#              #           #   #   #   #      #   #",
             "#              #       #   #   #       ## #####   #",
             "#              #       #   #       #####          #",
             "#              #       #   ## ##       #      #####",
             "#              #       #   #   #   #   #      #   #",
             "#              #       #           #   #      #   #",
             "#              #       #####   #   #####      #   #",
             "#              #       #       #              ### #",
             "#              #       #   ### #   #   ########   #",
             "#                      #           #   #          #",
             "#              ## ## ##### #   #   ### #      #   #",
             "# #### ###     #   #   #   #   #       #      ### #",
             "#    #   #     #   #       #   #   #   #      #   #",
             "#        #     #       #   #   #   #   #      #   #",
             "#    #   #         #   #       #   #   #          #",
             "## #### ##     ## #### ############### #### ##### #",
             "#        #                          #    #        #",
             "#        #                          #    #    #   #",
             "#                                   #    #    #   #",
             "#        #                          #    # ##### ##",
             "#        #                               #   #    #",
             "#        #                         #### ##        #",
             "#        #                         #         #    #",
             "#        ## #######                #     ##########",
             "#        #    #   #                #     #   #    #",
             "#                 #                #     #   #    #",
             "#        #    #   #                #         #    #",
             "#        #    #   #                #     #   #    #",
             "#        #    #   #                #     #        #",
             "###################################################"]

testArray :: UArray Coord Bool
testArray = lists2dToArray $ map (map (==' ')) $ testLevel

test f = f testArray (Coord 1 1) (Coord 16 4)

renderPathA :: (UArray Coord Bool -> Coord -> Coord -> Maybe [Coord]) -> Array Coord Char
renderPathA f = 
    case test f of 
      Nothing -> error "failed to find path"
      Just xs -> lists2dToArray testLevel // zip xs (cycle ['0'..'9'])

showPath f = mapM_ putStrLn $ array2dToLists (renderPathA f)

toCUArray :: Array Coord Bool -> UArray Coord Bool
toCUArray a = listArray (bounds a) (elems a)

pathfindWith :: (UArray Coord Bool -> Coord -> Coord -> Maybe [Coord])
             -> PathfindingProblem 
             -> Maybe [Coord]
pathfindWith f p@(PathfindingProblem start end grid) =
    (start :) `fmap` f (toCUArray grid) start end

renderSolutionWith :: (UArray Coord Bool -> Coord -> Coord -> Maybe [Coord])
             -> PathfindingProblem 
             -> Array Coord Char
renderSolutionWith f p@(PathfindingProblem start end grid) = 
    case f (toCUArray grid) start end of
      Nothing -> error "failed to find path"
      Just xs -> (amap (\c -> if c then '.' else '*') $ grid) 
                 // zip xs (cycle ['0'..'9'])

showSolutionWith f p = mapM_ putStrLn $ array2dToLists $ renderSolutionWith f p

optimizedAStar = astar
referenceAStar = caleAStar

data PathfindingProblem = PathfindingProblem {
      pstart :: Coord
    , pend :: Coord
    , pgrid :: Array Coord Bool } deriving Show

instance Arbitrary PathfindingProblem where
    arbitrary = do
      grid <- arbitrary
      start <- choose (bounds grid)
      end <- choose (bounds grid)
      return $ PathfindingProblem start end grid

instance (Arbitrary e) => Arbitrary (Array Coord e) where
    arbitrary = do
      bsHigh <- sized $ \n -> resize (round (sqrt $ fromIntegral n)) arbitrary `suchThat` (\h -> let size = rangeSize (Coord 0 0, h) in size > 0 && size < 100)
      elements <- vectorOf (rangeSize (Coord 0 0, bsHigh)) arbitrary
      return $ listArray (Coord 0 0, bsHigh) elements

{-
instance (Ix i, Arbitrary i, Arbitrary e) => Arbitrary (Array i e) where
    arbitrary = do
      bsLow <- sized $ \n -> resize (round (sqrt $ fromIntegral n)) arbitrary
      bsHigh <- sized $ \n -> resize (round (sqrt $ fromIntegral n)) arbitrary `suchThat` (\h -> let size = rangeSize (bsLow, h) in size > 0 && size < 100)
      elements <- vectorOf (rangeSize (bsLow, bsHigh)) arbitrary
      return $ listArray (bsLow, bsHigh) elements
-}
neighbours (Coord x y) =
    [Coord (x + 1) (y + 1),
     Coord (x + 1) (y + 0),
     Coord (x + 1) (y - 1),
     Coord (x + 0) (y - 1),
     Coord (x - 1) (y - 1),
     Coord (x - 1) (y + 0),
     Coord (x - 1) (y + 1),
     Coord (x + 0) (y + 1)]

isDiagonal (Coord ax ay) (Coord bx by) =
    (ax /= bx) && (ay /= by)

caleAStar :: UArray Coord Bool -> Coord -> Coord -> Maybe [Coord]
caleAStar a start end@(Coord endX endY) =
    Cale.aStar (Set.fromList . filter (\c -> inRange (bounds a) c && (a ! c)) . neighbours)
               (\x y -> if isDiagonal x y then sqrt 2 else 1)
               (\(Coord x y) -> fromIntegral (max (abs (endX - x)) (abs (endY - y))))
               (==end)
               start