{-# LANGUAGE FlexibleContexts, PatternGuards, NoMonomorphismRestriction, ScopedTypeVariables #-}
module Util where
-- todo: Split up general utility functions, ones based on BasicTypes, and ones based on all the rest, etc. so forth to avoid cyclic dependencies

import BasicTypes

import Control.Monad.Random.Class (getRandom, getRandomR)

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (foldl', sortBy, group, sort, intercalate, intersperse)
import Data.Char (isSpace)

import Data.Ord (comparing)

import Control.Monad.Writer (MonadWriter, tell)
import Control.Monad.Random (MonadRandom)

-- todo: IArray-ize? Not that I'm using a lot of other IArrays than Array...
import Data.Array.IArray (IArray, Array, inRange, bounds, (!), listArray, array, elems)
import Data.Ix

import Data.Graph.Inductive (match, nodes, (&))

import Data.Accessor (Accessor, accessor)

-- | Euclidean distance between 'Coord's. This is this game's official distance metric, others should only be used when there's a good reason (for instance, if they're faster and don't change game behaviour).
euclideanDistance :: Coord -> Coord -> Double
euclideanDistance = (sqrt .) . squaredEuclideanDistance

-- | Squared Euclidean distance between 'Coord's. Faster to compute than the Euclidean distance. Useful when you only need to compare Euclidean distances but not add/subtract them etc.
squaredEuclideanDistance :: Coord -> Coord -> Double
squaredEuclideanDistance (Coord x1 y1) (Coord x2 y2) =
    (fromIntegral (x1 - x2)) ** 2 + (fromIntegral (y1 - y2)) ** 2

-- | Chebyshev distance. Useful when you need a quick admissible distance heuristic. Returns Int!
chebyshevDistance :: Coord -> Coord -> Int
chebyshevDistance (Coord x1 y1) (Coord x2 y2) =
    max (abs (x1 - x2)) (abs (y1 - y2))

-- | Naive shuffle
--shuffle :: (MonadState GameState m) => [a] -> m [a]
shuffle xs =
    do rs <- mapM (const getRandom) xs
       return $ map fst $ sortBy (comparing snd) $ zip xs (rs :: [Int])

-- | Run an action on a Just value, do nothing on a Nothing.
onJust Nothing _ = return ()
onJust (Just x) act = act x

-- | Variant of when that runs an action to determine whether to run an action.
whenM p a = do doIt <- p; when doIt a

unlessM p a = do doIt <- p; unless doIt a

ifM p t e = do doIt <- p
               if doIt
                  then t
                  else e

-- | Randomly decide whether to take an action or not. Example: (5 `timesOutOf` 6) $ liftIO $ print "hi"
--timesOutOf :: (Num a, Random a, MonadState GameState m, Ord a) => a -> a -> m () -> m ()
timesOutOf a b act =
    do r <- getRandomR (1, b)
       when (a >= r) act

probOutOf a b =
    do r <- getRandomR (1, b)
       return (a >= r)

--probOneOutOf :: MonadState GameState m => Int -> m Bool
probOneOutOf p =
    do r <- getRandomR (1, p)
       return (r == 1)

-- | Safely convert a list's head into a Just, Nothing on an empty list
-- todo: replace with listToMaybe everywhere
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

(!!/) :: [a] -> Int -> Maybe a
[] !!/ n = Nothing
(x:xs) !!/ 0 = Just x
(x:xs) !!/ n = xs !!/ (n - 1)

-- | Look up a set in a map of sets, returns an empty set if no set was found. Equivalently, get the adjacencies of a node in a graph represented in the obvious way.
lookupAssocSet :: Ord k => k -> Map k (Set a) -> Set a
lookupAssocSet k m = 
    case Map.lookup k m of
      Nothing -> Set.empty
      Just x -> x

insertAssoc :: (Ord a, Ord k) => k -> a -> Map k (Set a) -> Map k (Set a)
insertAssoc k v =
    insertAssocSet k (Set.singleton v)

insertAssocSet :: (Ord a, Ord k) => k -> Set a -> Map k (Set a) -> Map k (Set a)
insertAssocSet k v =
    Map.insertWith' Set.union k v

removeAssoc k v =
    removeAssocSet k (Set.singleton v)

removeAssocSet k v =
    Map.alter (\s -> case s of 
                       Nothing -> Nothing
                       Just x -> let newX = Set.difference x v in
                                 if Set.null newX
                                 then Nothing
                                 else Just newX) k


-- | Remove the given value m from all of the sets sets, in a map of sets (the last argument)
removeFromAssocs :: (Ord a, Ord k) => a -> Set k -> Map k (Set a) -> Map k (Set a)
removeFromAssocs a sets =
    let me = Set.singleton a in
    Map.unionWith Set.difference $
    Map.fromAscList [(k, me) | k <- Set.toAscList sets]

-- | Remove the value k from all of the sets in a map of sets, if m ! k is the set of all sets containing k. For instance if m is an undirected graph.  
removeFromSymAssocs k m =
    case Map.lookup k m of
      Nothing -> m
      Just x -> removeFromAssocs k x m
--    removeFromAssocs k (m Map.! k) m

-- | Add k to the given sets in m
addToAssocs :: (Ord a, Ord k) => a -> Set k -> Map k (Set a) -> Map k (Set a)
addToAssocs k sets m =
    let kSet = Set.singleton k in
    Map.unionWith Set.union m $
    Map.fromAscList [(k, kSet) | k <- Set.toAscList sets]

filterMset :: (Monad m, Eq a) => (a -> m Bool) -> Set a -> m (Set a)
filterMset f s =
    do vs <- filterM f $ Set.toAscList s
       return $ Set.fromAscList vs -- 


none :: (a -> Bool) -> [a] -> Bool
none = (not .) . any

allApply :: [a -> Bool] -> a -> Bool
allApply fs x = all ($x) fs

anyApply :: [a -> Bool] -> a -> Bool
anyApply fs x = any ($x) fs

castEnum :: (Enum a, Enum b) => a -> b
castEnum = toEnum . fromEnum

minIndexBy :: (a -> a -> Ordering) -> [a] -> Int
minIndexBy _ [] = error "minIndexBy: empty list"
minIndexBy cmp xss@(x:_) = (\(_, x, _) -> x) $ foldl' go (0, 0, x) xss where
    go (index, minIndex, minVal) val | cmp val minVal == GT = (index + 1, index, val)
                                     | otherwise = (index + 1, minIndex, minVal)

minIndex :: Ord a => [a] -> Int
minIndex = minIndexBy compare

applyOnIndex :: Int -> (a -> a) -> [a] -> [a]
applyOnIndex n f xs = 
    let (begin, end) = splitAt n xs in
    case end of
      [] -> begin -- arguably undefined but I guess I'm gonna go with this
      (x:xs) -> begin ++ (f x : xs)

listIndex :: Int -> Accessor [a] a 
listIndex i = accessor (!! i) (applyOnIndex i . const)

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
spanM _ xs@[] = return (xs, xs)
spanM p xs@(x:xs') =
    do r <- p x
       case r of
         True -> do (ys, zs) <- spanM p xs'
                    return (x:ys, zs)
         False -> return ([], xs)


breakM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
breakM p = spanM (liftM not . p)

inclusiveWords                   :: String -> [String]
inclusiveWords [] = [[]]
inclusiveWords xs =
    case (evalState (breakM go xs) False) of
      (these, []) -> [these]
      (these, nextOnes) -> these : inclusiveWords nextOnes
    where
      go x =
          do s <- get
             case (s, isSpace x) of
               (False, True) -> put True >> return False
               (True, False) -> return True
               (_, _) -> return False

-- | Take members of xs, with each member's contribution being determined by the contribution function, stopping *before* you hit the limit
-- | returns ((members before limit, members after limit), sum contribution of members before limit)
-- funny generalization idea: Replace the (+) with mappend, replace the limit with a function determining whether we should stop based on the mappended values so far
takeUntilLimit :: (Ord b, Num b) => (a -> b) -> b -> [a] -> (([a], [a]), b)
takeUntilLimit contribution limit xs =
    flip runState 0 $
    flip breakM xs $ \w -> 
        get >>= \s ->
        if s + contribution w > limit 
        then return True
        else modify (+ contribution w) >> return False

explode :: Eq a => a -> [a] -> [[a]]
explode d xs = let (begin, end) = break (==d) xs in
               begin : case (drop 1 end) of
                 [] -> []
                 xss -> explode d xss
               
groupByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM _ [] = return []
groupByM eq (x:xs) = do 
  (ys,zs) <- spanM (eq x) xs 
  gs <- groupByM eq zs
  return ((x:ys) : gs)

matching f x y = (f x) == (f y)

unpick :: Int -> [Int] -> [Bool]
unpick n xs = let l = go 0 xs in l ++ (replicate (n - length l) False) 
    where 
      go _ [] = []
      go acc xss@(x:xs) 
          | acc == x = True : go (acc+1) xs
          | otherwise = False : go (acc+1) xss

pick :: [Bool] -> [a] -> [a]
pick = ((map snd . filter fst) .) . zip

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
    case f x of
      Nothing -> []
      Just r -> r : iterateMaybe f r

groupN :: [a] -> Int -> [[a]]
groupN [] _ = []
groupN xs n = a : groupN b n where
      (a,b) = splitAt n xs

lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast 0 _ = True
lengthAtLeast _ [] = False
lengthAtLeast n (_:xs) = lengthAtLeast (n - 1) xs

-- "chained" foldr for things like updating maps with multiple folds
cfoldr :: (a -> b -> b) -> [a] -> b -> b
cfoldr f = flip (foldr f)

-- | monadic version of scan
scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m [a]
scanM _ q [] = return [q]
scanM f q (x:xs) =
   do q2 <- f q x
      qs <- scanM f q2 xs
      return (q:qs)

-- | For quickly changing a computation to pick up a value from inside it, using MonadWriter
grab :: MonadWriter w m => w -> m w
grab w = do tell w; return w

adjustAssocList :: Eq k => (a -> a) -> k -> [(k, a)] -> [(k, a)]
adjustAssocList _ _ [] = []
adjustAssocList f k (le@(lk, x):xs) 
    | lk == k = (lk, f x) : xs
    | otherwise = le : adjustAssocList f k xs

nubOrd :: Ord a => [a] -> [a]
nubOrd = map head . group . sort

recursiveFringe :: forall a b. (a -> (b, [a])) -> a -> [b]
recursiveFringe f x = go x [] where
    go :: a -> [b] -> [b]
    go x = 
        let (r, ds) = f x in
        case ds of
          [] -> ([r] ++)
          (_:_) -> foldr1 (.) (map go ds)

recursiveFringeM :: forall a b m. Monad m => (a -> m (b, [a])) -> a -> m [b]
recursiveFringeM a x = go x [] where
    go :: a -> [b] -> m [b]
    go x xs =
        do (r, ds) <- a x
           case ds of
             [] -> return ([r] ++ xs)
             (_:_) -> foldr1 (>=>) (map go ds) $ xs

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs
foldM1 _ [] = fail "Empty list in foldM1"



array2dToLists a = 
    let (Coord beginX beginY, Coord endX endY) = bounds a in
    [[a ! Coord x y | x <- [beginX..endX]] | y <- [beginY..endY]]

lists2dToArray [] = array (Coord 0 0, Coord (-1) (-1)) []
lists2dToArray xs =
    let xe = length (head xs) - 1
        ye = length xs - 1
    in array (Coord 0 0, Coord xe ye) $ zip [Coord x y | y <- [0..ye], x <- [0..xe]] $ concat xs

adjacentAtDistance n (Coord aX aY) =
    [Coord x (aY + n) | x <- [aX - n .. aX + n]] ++
    [Coord x (aY - n) | x <- [aX - n .. aX + n]] ++
    [Coord (aX + n) y | y <- [aY - n + 1 .. aY + n - 1]] ++
    [Coord (aX - n) y | y <- [aY - n + 1 .. aY + n - 1]]



uniformRandomPick :: MonadRandom m => [a] -> m (Maybe (a, [a]))
uniformRandomPick [] = return Nothing
uniformRandomPick xs = 
    let l = length xs in
    do p <- getRandomR (0, l - 1)
       return $ Just (xs !! p, take p xs ++ drop (p + 1) xs)

justUniformRandomPick :: MonadRandom m => [a] -> m (a, [a])
justUniformRandomPick xs = 
    let l = length xs in
    do p <- getRandomR (0, l - 1)
       return $ (xs !! p, take p xs ++ drop (p + 1) xs)

--weightedRandomPick :: (MonadRandom m, Num n, Ord n, Random n) => [(n, a)] -> m (Maybe a)
weightedRandomPick [] = return Nothing
weightedRandomPick xs =
    let s = sum $ map fst xs in
    do p <- getRandomR (0, s)
       return $ Just $ snd $ (case snd $ fst $ takeUntilLimit fst p xs of
                                [] -> last xs -- todo: is this right?
                                (x:xs) -> x)

takeWhileAndOne p [] = []
takeWhileAndOne p (x:xs) | p x = x : takeWhileAndOne p xs
                         | otherwise = x : []

-- xxx: depends on f to consume at least one item off the list on each call in order to halt
foldPart :: ([a] -> ([a], [a])) -> [a] -> [[a]]
foldPart f xs = 
    case f xs of
      (as, []) -> [as]
      (as, bs) -> as : foldPart f bs

-- ^ same as gmap, except "double-counts" edges - i.e. each edge is seen both in the context where it's outgoing and in the one where it's incoming. Unpredictable results if you try to change the graph structure!
-- ^ Note the type restriction. I can't seem to come up with a nice way to make a double-counting gmap where you can actually change the node and/or edge types. One very un-nice possibility I could come up with is starting with an empty new graph and putting in the nodes first and edges later, but man, that's ugly and not very nice at all
-- in fact this is a rather nice example of the generally rather rare problem in Haskell that you actually honestly really want to do something and the type system is simply preventing you from doing it
gmapDouble f g =
    foldr (\n g -> (case match n g of (Just ctx, gPart) -> f ctx & gPart
                                      (Nothing, _) -> error "gmapDouble: someone broke fgl :(")) g $ nodes g

gselDouble p g =
    foldr (\n ns -> (case match n g of (Just ctx, gPart) -> if p ctx then n:ns else ns
                                       (Nothing, _) -> error "gselDouble: someone broke fgl :(")) [] $ nodes g

ntimes 0 f acc = acc
ntimes n f acc = ntimes (n - 1) f (f acc)

ntimesM 0 a acc = return acc
ntimesM n a acc = a acc >>= \r -> ntimesM (n - 1) a r

(!/) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
a !/ c | inRange (bounds a) c = Just (a ! c)
       | otherwise = Nothing

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM p [] = return []
takeWhileM p (x:xs) =
    do ok <- p x
       case ok of
         True -> do rest <- takeWhileM p xs
                    return (x:rest)
         False -> return []

newIArray :: (Ix i, IArray a e) => (i, i) -> e -> a i e
newIArray bs e = listArray bs $ replicate (rangeSize bs) e

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p xs = or `liftM` (sequence $ map p xs)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM p [] = return Nothing
findM p (x:xs) = do
  ok <- p x
  case ok of
    True -> return (Just x)
    False -> findM p xs
                
renderCoordArray :: Show s => Array Coord s -> String
renderCoordArray a =
    let b = bounds a in
    intercalate "\n" $ map (\l -> intercalate " " $ map show l) $ array2dToLists a

renderCoordCharArray :: Array Coord Char -> String
renderCoordCharArray a =
    let b = bounds a in
    intercalate "\n" $ array2dToLists a