{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
module Los (calculateFov, allConnectingLines) where

import BasicTypes
import Data.List (group)

type Bump = Coord

bres run rise
    | run  < 0   = (map . map) (\(x, y) -> (-x, y)) (bres (-run) rise)
    | rise < 0   = (map . map) (\(x, y) -> (x, -y)) (bres run  (-rise))
    | rise > run = (map . map) (\(x, y) -> (y,  x)) (bres rise   run)
    | otherwise  = map (go run) [0..run-1]
    where
      go p eps = zip [0..p] . map fst $ iterate step (0, eps)
      step (y, error)
          | error' >= run = (y + 1, error' - run)
          | otherwise  = (y,     error')
          where error' = error + rise

allConnectingLines  (Coord x1 y1) (Coord x2 y2) = 
    map head . group .
    (map . map) (\(x, y) -> Coord (x1 + x) (y1 + y)) $ (bres (x2-x1) (y2-y1))

data Line = Line
    {-# UNPACK #-} !Coord
    {-# UNPACK #-} !Coord deriving (Eq, Ord)

near (Line l _) = l
far (Line _ f) = f

data View = View {-# UNPACK #-} !Line {-# UNPACK #-} !Line ![Bump] ![Bump]
    deriving (Eq, Ord)

relativeSlope (Line (Coord nearX nearY) (Coord farX farY)) (Coord pX pY) =
    (farY - nearY) * (farX - pX) - (farX - nearX) * (farY - pY)

isBelow, isBelowOrContains, isAbove, isAboveOrContains, containsPoint :: Line -> Coord -> Bool
isBelow l p = relativeSlope l p > 0
isBelowOrContains l p = relativeSlope l p >= 0
isAbove l p = relativeSlope l p < 0
isAboveOrContains l p = relativeSlope l p <= 0
containsPoint l p = relativeSlope l p == 0

--nextCoord :: Int -> Int -> Bool -> [View] -> Maybe Coord
nextCoord rank file
    | file == rank = Nothing
    | otherwise = Just ((Coord (rank - nextFile) nextFile), nextFile)
    where 
          nextFile = file + 1

firstCoordOnRank :: Int -> Coord
firstCoordOnRank rank = Coord rank 0

{-
  Let's consider matters of control again. What can visitTile tell about which views should be given to it in its next invocation, and what views should be 
  saved for the next rank (instead of being removed due to being blocked oslt)?

- assumption: visitTile only needs to be given one view. That view will never be below the checked coord (steepLine belowOrContains bottomRight), and will be the first view not to be below it
- thus:
 - if we went above the view, the view given to us should be kept for the next rank, but we should be given the next view for the next file
 - if we were blocked, the view given to us should be removed, and we should be given the next view for the next file
 - if were shallow bumped, the view given to us should be updated, and we should be given the *same* view for the next file
 - if we were steep bumped, the view given to us should be updated, and we should be given the *next* view for the next file
 - if we were in-betweened, the view given to us should be replaced with two views, and we should be given the steeper one for the next file

Thus: We are given a view which is always removed from the list of views. We return a list of views which will replace it, and a bool to tell whether the first one in that list should be skipped.

-}

-- bool1: skip the first of the returned views? (if no views are returned, we automatically go to the next one in any case)
-- bool2: should we just go to the next tile instead of trying to skip some?

--visitTile :: (Coord -> Game ()) -> Bool -> Coord -> View -> Game (Bool, Bool, [View])
visitTile visitAction b coord@(Coord destX destY) v@(View shallowLine steepLine _ _) 
    | shallowLine `isAbove` bottomRight && steepLine `isBelow` topLeft =
          visitAction coord >> return (if b
                                       then (False, True, [])
                                       else (True, False, [v]))

    | shallowLine `containsPoint` bottomRight && steepLine `containsPoint` bottomRight = 
        return (True, False, [v])

    | shallowLine `isAboveOrContains` topLeft =
        return (False, False, [v])

    | steepLine `isBelowOrContains` bottomRight =
        return (True, False, [v])

    | shallowLine `isAbove` bottomRight =
          visitAction coord >> 
          return (if b
                  then (goIfChecked (addShallowBump v topLeft) False b)
                  else (False, False, [v]))

    | steepLine `isBelow` topLeft =
          visitAction coord >> 
          return (if b
                  then (goIfChecked (addSteepBump v bottomRight) True b)
                  else (True, False, [v]))

    | otherwise = 
        visitAction coord >> 
        return (if b
                then goIfCheckedTwo (splitView v topLeft bottomRight) b
                else (False, False, [v]))
    where
      topLeft = Coord destX (destY + 1)
      bottomRight = Coord (destX + 1) destY

      goIfChecked v skip bumped | checkView v = (skip, bumped, [v])
                                | otherwise = (False, bumped, [])

      goIfCheckedTwo (v1, v2) bumped
          | c1 && c2 = (True, bumped, [v1, v2])
          | c1 = (True, bumped, [v1])
          | c2 = (False, bumped, [v2])
          | otherwise = (False, bumped, [])
            where c1 = checkView v1
                  c2 = checkView v2



addShallowBump (View (Line shallowNear _) steepLine shallowBumps steepBumps) coord =
    n
    where newShallowLine1 = Line shallowNear coord
          newShallowBumps = coord : shallowBumps
          newShallowLine2 = foldr (\coord line -> 
                                       if line `isAbove` coord 
                                       then Line coord (far line) 
                                       else line) 
                            newShallowLine1 steepBumps
          n = View newShallowLine2 steepLine newShallowBumps steepBumps

addSteepBump (View shallowLine (Line steepNear _) shallowBumps steepBumps) coord =
    n
    where newSteepLine1 = Line steepNear coord
          newSteepBumps = coord : steepBumps
          newSteepLine2 = foldr (\coord line -> 
                                     if line `isBelow` coord 
                                     then Line coord (far line) 
                                     else line) 
                          newSteepLine1 shallowBumps
          n = View shallowLine newSteepLine2 shallowBumps newSteepBumps

checkView (View shallowLine steepLine _ _) =
    not $
    shallowLine `containsPoint` near steepLine &&
    shallowLine `containsPoint` far steepLine &&
    (shallowLine `containsPoint` (Coord 0 1) || shallowLine `containsPoint` (Coord 1 0))


splitView v topLeft bottomRight =
    (shallower, steeper)
    where
      shallower = addSteepBump v bottomRight
      steeper = addShallowBump v topLeft

startingView = View (Line (Coord 0 1) (Coord 1000 0))
                    (Line (Coord 1 0) (Coord 0 1000))
                    [] []

calculateQuadrant visitAction blocked (sX, sY) quadrantFun =
    loop [startingView] 1
 
    where
      adjust = (\(x, y) -> Coord (x + sX) (y + sY)) . quadrantFun
      adjustedBlockAction = blocked . adjust
      adjustedVisitAction = visitAction . adjust

      loop [] _  = return ()
      loop views rank = 
          do v <- doRank rank 0 (firstCoordOnRank rank) views []
             loop v (rank + 1)

      doRank rank file coord viewsFront viewsBack =
          do b <- adjustedBlockAction coord

             (skipView, _, givenViews) 
                 <- visitTile adjustedVisitAction b coord (head viewsFront)

             let (newViewsFront, newViewsBack) = 
                     case (skipView, givenViews) of
                       (True, (x:xs)) -> 
                           (xs ++ tail viewsFront, x : viewsBack)
                       (False, xss@(_:_)) -> 
                           (xss ++ tail viewsFront, viewsBack)
                       (_, []) -> (tail viewsFront, viewsBack)

             case (newViewsFront, nextCoord rank file) of
               ([], _) -> return (reverse newViewsBack)
               (_, Nothing) -> return (reverse newViewsBack)
               (_, Just (nextCoord, nextFile)) -> 
                   doRank rank nextFile nextCoord newViewsFront newViewsBack


calculateFov visitAction blocked start =
    visitAction start >>
    mapM_ (calculateQuadrant visitAction blocked (fromCoord start))
              [\(Coord a b) -> (a * x, b * y) | y <- [1, -1], x <- [1, -1]]

-- toCoord (x, y) = Coord x y
fromCoord (Coord x y) = (x, y)

--calculateFovQuadrants :: Monad m => (Coord -> m ()) -> (Coord -> m Bool)
--                         -> Coord -> [Int] -> m ()
{-
calculateFovQuadrants visitAction blocked start qs =
    visitAction start >>
    mapM_ (calculateQuadrant visitAction blocked start) 
          (map ([\(a, b) -> (a * x, b * y) | y <- [1, -1], x <- [1, -1]]!!) qs)
-}
{-

OK, let's see if I'm starting to finally gain some kind of an understanding of this algorithm of shit.

Firstly, our coordinate system is shifted by (-0.5 *** -0.5). This means that we can find the upper left edge of the origin at (0, 1), and the lower right edge at (1, 0). 

We thread a state consisting of a list of views in the algorithm while going outwards tile by tile. The order that we check tiles is the same as the order the views are sorted, and views are mutually exclusive, so for each tile, we should only need to check one field.

Let's break it up into single traversals over a diagonal line of tiles. This is significant here because over a traversal, we can zipper over the list of views, reversing it along the way, thus always having access to the position on the list where elements are to possibly be added or removed. As the views get reversed, we'll then come back the other way, boustrophedonically. The views will be kept sorted if the local behaviour is correct.

There's a further possible optimisation... though it's probably going to be very situation-dependent (I see it being quite useful in cramped rooms and terrible in, say, forests). If a tile causes a steep bump (late bump?) or is not within any views, compute the intersection of the currently taken diagonal line, and of the shallow (early?) line of the next view, to find the next tile to look at. Calculating this might require division and rounding, though, so beware weirdness!


-}


-- takeWhileM :: Monad m => (a -> m Bool) -> [m a] -> m [a]
-- takeWhileM p [] = return []
-- takeWhileM p (m:ms) = do
-- 	x <- m
--         r <- p x
-- 	if r
-- 	 then liftM (x:) $ takeWhileM p ms
--          else return []

-- mapMuntil :: (Monad m) => (a -> m Bool) -> (a1 -> m a) -> [a1] -> m [a]
-- mapMuntil p f = takeWhileM p . map f

