{-# LANGUAGE FlexibleContexts, ParallelListComp, TupleSections, NoMonomorphismRestriction, ScopedTypeVariables, NamedFieldPuns, BangPatterns #-}
module Interface (renderPos, doInterface) where

import Prelude hiding (catch)
import Control.Exception (catch, SomeException)
import Control.Concurrent
import Control.Arrow ((&&&), (***), first, second)
import Data.List (find, sortBy, groupBy, findIndex, intersperse, sort, intersect, minimumBy, (\\), intercalate)
import Control.Monad.State
import Game
import BasicTypes
import Object
import RenderObject
import CommonTypes
import qualified LocationMap as Loc
import AI (act)
import Action
import Interface.Key
import Text
import CursesWrap
import UI.HSCurses.Curses (Key(..))
import Util (none, whenM, unlessM, ifM, matching, unpick, pick, applyOnIndex, 
             takeUntilLimit, maybeHead, lengthAtLeast, (!!/),
             takeWhileM, anyM)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, listToMaybe, catMaybes, isNothing, fromMaybe)

import Control.Monad.Writer
import Los
import Data.Accessor
import Data.Array.IArray

import qualified Data.Set as Set
import qualified Data.Map as Map

import Tile
import SaveLoad (saveToFile)
import Data.Default

keyToDirection (KeyChar '1') = Just SW
keyToDirection (KeyChar '2') = Just S
keyToDirection (KeyChar '3') = Just SE
keyToDirection (KeyChar '4') = Just W
keyToDirection (KeyChar '6') = Just E
keyToDirection (KeyChar '7') = Just NW
keyToDirection (KeyChar '8') = Just N
keyToDirection (KeyChar '9') = Just NE
keyToDirection (KeyA1) = Just NW
keyToDirection (KeyA3) = Just NE
keyToDirection (KeyC1) = Just SW
keyToDirection (KeyC3) = Just SE
keyToDirection (KeyDown) = Just S
keyToDirection (KeyUp) = Just N
keyToDirection (KeyLeft) = Just W
keyToDirection (KeyRight) = Just E
keyToDirection _ = Nothing

selectDirection _ [] = return Nothing
selectDirection msg ds = do
  mpr ("Select direction" ++ msg ++ ": [" ++ map facingNumber ds ++ "]")
  c <- keyToDirection `fmap` liftIO getCh
  return $ join $ find (==c) (map Just ds)

onMapScreen (Coord sX sY) (Coord x y) = (x - sX + 7, y - sY + 7)
fromMapScreen (Coord sX sY) (x, y) = Coord (x + sX - 7) (y + sY - 7)

mapScreen = ((0, 0), (15, 15))
fullScreen = ((0, 0), (79, 23))
messageScreen = ((0, 16), (79, 23))

screenHeight s = (snd $ snd s) - (snd $ fst s)
screenWidth s = (fst $ snd s) - (fst $ fst s)
screenPos = fst
screenDownLeftPos s = (fst $ fst s, snd $ snd s)

printLinesReverse (eX, eY) =
    zipWithM_ (\lineNum line ->
                   mvAddStr eX (eY - lineNum) line) [0..] 


updateMessageScreen = do
  m <- gsGlobal messages_
  let messagesToRender = fst $ fst $ 
                         takeUntilLimit length (screenHeight messageScreen) $ 
                         map (wordWrap (screenWidth messageScreen)) m
  liftIO $ uncurry clearArea messageScreen
  liftIO $ printLinesReverse (screenDownLeftPos messageScreen) $ 
         concat messagesToRender
  liftIO $ refresh


data TargetArgs m = TargetArgs {
      source :: Maybe Coord
    , beamFun :: (Coord -> Coord -> GameT m [Coord])
    , statusFun :: ([Coord] -> GameT m ()) 
    , acceptFun :: ([Coord] -> GameT m Bool)
    , targets :: [Coord]
}

instance (Functor m, Monad m) => Default (TargetArgs m) where
    def = TargetArgs {
            source = Nothing -- use player character position
          , beamFun = shootingBeam
          , statusFun = const $ return ()
          , acceptFun = const $ return True
          , targets = []
          }

target :: forall m.
          (MonadIO m, Functor m) =>
          TargetArgs m
       -> GameT m (Maybe [Coord])
target (TargetArgs {source, beamFun, statusFun, acceptFun, targets}) = do
    sourceCoord <- case source of
                     Just x -> return x
                     Nothing -> snd `fmap` mapPosition playerCharRef
    let go :: [Coord] -> Coord -> Coord -> [Coord] -> GameT m (Maybe [Coord])
        go repaint lastCoord targetCoord targets = do
          beamPath <- (targetCoord :) `fmap` beamFun sourceCoord targetCoord
          accepted <- acceptFun beamPath
          case (accepted || sourceCoord == targetCoord) of
            False -> go repaint lastCoord lastCoord targets
            _ -> do
              mapM_ (renderPosToScreen sourceCoord . 
                     (onMapScreen sourceCoord)) repaint
              liftIO $ withStyle (Style False False Grey Black) $ renderBeam sourceCoord beamPath
              statusFun beamPath
              liftIO $ refresh
              c <- liftIO getCh
              case keyToDirection c of
                Just d -> do newCoord <- adjustPosWithinWorld d targetCoord
                             go beamPath targetCoord newCoord targets
                Nothing -> 
                    case c of
                      KeyEnter -> return $ Just beamPath
                      KeyChar 't' -> case targets of 
                                       [] -> go [] lastCoord targetCoord targets
                                       (x:xs) -> go beamPath targetCoord x (xs ++ [x])
                      KeyChar ' ' -> return $ Just beamPath
                      KeyChar '\r' -> return $ Just beamPath
                      KeyChar 'f' -> return $ Just beamPath
                      KeyChar 'q' -> return Nothing
                      _ -> go [] lastCoord targetCoord targets

    case targets of
      [] -> go [] sourceCoord sourceCoord []
      (x:xs) -> go [] x x (xs ++ [x])

rotateList [] = []
rotateList (x:xs) = xs ++ [x]

adjustPosWithinWorld d c =
    do b <- bounds `fmap` gsPlayerLevel world_
       let newC = adjustPos d c
       if inRange b newC
          then return newC
          else return c

renderBeam :: Coord -> [Coord] -> IO ()
renderBeam origin xs =
    do zipWithM_ (uncurry mvAddCh . onMapScreen origin) xs . 
                 map (\(from, to) -> 
                      maybe ' ' 
                            facingArrow 
                            (directionOfAdjacentCoord from to)) .
                 ap zip tail $ xs
       uncurry mvAddCh (onMapScreen origin (last xs)) 'X'

facingArrow :: Facing -> Char
facingArrow E = '-'
facingArrow W = '-'
facingArrow N = '|'
facingArrow S = '|'
facingArrow NE = '/'
facingArrow NW = '\\'
facingArrow SE = '\\'
facingArrow SW = '/'

facingNumber :: Facing -> Char
facingNumber f = 
    case f of
      SW -> '1'
      S -> '2'
      SE -> '3'
      W -> '4'
      E -> '6'
      NW -> '7'
      N -> '8'
      NE -> '9'

directionOfAdjacentCoord (Coord x1 y1) (Coord x2 y2) 
    | y1 == y2 && x1 > x2 = Just W
    | y1 == y2 && x1 < x2 = Just E
    | x1 == x2 && y1 > y2 = Just N
    | x1 == x2 && y1 < y2 = Just S
    | x1 > x2 && y1 > y2 = Just NW
    | x1 > x2 && y1 < y2 = Just SW
    | x1 < x2 && y1 > y2 = Just NE
    | x1 < x2 && y1 < y2 = Just SE
    | otherwise = Nothing


renderPos :: (Monad m, Functor m) => 
             Bool
          -> Coord
          -> GameT m StyledChar

renderPos showEverything pos = do
  world <- gsPlayerLevel world_
  positions <- gsGlobal positions_
  references <- gsGlobal references_
  explored <- gsPlayerLevel playerExplored_
  visible <- gsPlayerLevel playerVisible_
  remembered <- gsPlayerLevel playerRemembered_
  flinged <- gsPlayerLevel flinged_
  level <- playerLevel
  flingedObjs <- 
      mapM dereferObjToSpecific $ 
      mapMaybe (\x -> do 
                  h <- flingedPath_ x !!/ flingedProgress_ x
                  case h == pos of
                    False -> Nothing
                    True -> Just $ flingedObject_ x) flinged
  case inRange (bounds world) $ pos of
    False -> return $ StyledChar (Style False False Black Black) ' '
    True ->
        let specObjs = Set.toAscList 
                       $ foldr Set.delete (lookupObjs references positions (OnMap level pos)) 
                       $ map (flip SpecificObject (error "renderPos: I can't make up SpecificObjects anymore?") . flingedObject_) $ flinged

            features
                | showEverything || tileIsTrue explored pos = [world ! pos]
                | otherwise = []
            objects 
                | showEverything || Set.member pos visible =
                    sortBy (comparing (objectRenderingOrder . obj)) $ (flingedObjs ++ specObjs)
                | otherwise = []
        in
          case objects of
            (x:_) -> renderObject x
            [] -> if (explored ! pos) && not (pos `Set.member` visible)
                  then return $ StyledChar (Style True False Black Black) (remembered ! pos)
                  else case features of
                         (x:_) -> return $ renderTile x
                         [] -> return $ StyledChar (Style False False Black Black) ' '

renderPosToScreen charPos 
                  pos = do
  c <- renderPos False
                 (fromMapScreen charPos pos)
  liftIO $ do uncurry move pos
  renderStyledChar c
  return ()

renderMapPosToScreen pos = do
  charPos <- mapPosition playerCharRef
  c <- renderPos False pos
  let screenPos = onMapScreen (snd charPos) pos
  when (inRange mapScreen screenPos) $ 
       do liftIO $ uncurry move (onMapScreen (snd charPos) pos)
          renderStyledChar c
          return ()

renderStyledChar (StyledChar style c) =
    liftIO $ withStyle style $ addCh c
renderStyledChar (StyleAnimatedChar styles c) = do
    turn <- gsGlobal currentTurn_
    liftIO $ withStyle (styles !! (turn `mod` length styles)) $ addCh c

lookupTile a c 
    | bounds a `inRange` c = a ! c
    | otherwise = makeWall PlainWall

tileIsTrue a c
    | bounds a `inRange` c = a ! c
    | otherwise = False

headOrDefault d [] = d
headOrDefault _ (x:_) = x

doInterface =
    do mGlobal ((turnDone ^= False) . (doQuit ^= False))

       liftIO erase

       tWorld <- gsPlayerLevel world_
       charObj <- dereferObj playerCharRef
       charPos <- mapPosition playerCharRef

       tPositions <- gsGlobal positions_
       tReferences <- gsGlobal references_
       tRemembered <- gsPlayerLevel playerRemembered_
       tExp <- gsPlayerLevel playerExplored_
       tVis <- gsPlayerLevel playerVisible_
       tLevel <- playerLevel

       let recIfInBounds c | inRange (bounds tWorld) c = tell [c]
                           | otherwise = return ()
           blocksVision = return . posBlocksVision tWorld 
                                                   tReferences
                                                   tPositions
                                                   (snd charPos) . OnMap tLevel
           visible = snd charPos : 
                     (execWriter $ calculateFov recIfInBounds
                                                blocksVision
                                                (snd charPos))

       mPlayerLevel (playerExplored ^: \a -> accum (||) a $ map (,True) $ visible)
       let visibleSet = Set.fromList visible
       mPlayerLevel (playerVisible ^= visibleSet)
       newRemembered <- mapM (renderPos False) visible
       mPlayerLevel (playerRemembered ^: (// zip visible (map c newRemembered)))

       mapM_ (renderPosToScreen (snd charPos)) [(x, y) | x <- [0..16], y <- [0..16]]

       updateMessageScreen

       liftIO $ mvAddStr 37 2 ("Health:  "++ show (health_ $ charObj) ++ "/10")

       turn <- gsGlobal currentTurn_
       liftIO $ mvAddStr 37 5 ("Turn:     " ++ show turn)
       liftIO $ mvAddStr 37 6 ("Position: " ++ show charPos)

       focusSlotKeys <- map (keyToChar . fromMaybe (KeyChar '?')) `fmap` mapM focusSlotKey [0..numFocuses_ charObj]
       focusDescriptions <- mapM (name <=< dto) $ focus_ charObj

       let focusSlots = zipWith (\key descr -> key : ": " ++ descr) focusSlotKeys focusDescriptions

       liftIO $ zipWithM_ (\n slot -> mvAddStr 37 (8+n) slot) [0..] focusSlots

       animate



animate = do
  flingeds <- gsPlayerLevel flinged_
  liftIO refresh
  case flingeds of
    [] -> liftIO $ timeout (-1)
    (_:_) -> mPlayerLevel (flinged ^: updateFlings) >> liftIO (timeout 100)
  c <- liftIO $ wgetch stdScr
  case c of
    KeyUnknown (-1) -> renderFlings >> animate -- player didn't press anything, keep flings in the air
    _ -> finishFlings >> handleInput c

finishFlings = do
  mPlayerLevel (flinged ^: map (flingedProgress ^= maxBound))
  renderFlings

renderFlings = do 
  flingeds <- gsPlayerLevel flinged_
  mPlayerLevel (flinged ^= filter (\x -> lengthAtLeast (flingedProgress_ x + 1) $ flingedPath_ x) flingeds)
  mapM_ renderMapPosToScreen . concatMap (flingedPath_) $ flingeds

updateFlings =  map (flingedProgress ^: succ)

handleInput c = do
  charPos <- mapPosition playerCharRef
  o <- dereferObj playerCharRef
  keyAction <- getKeyAction c
  case keyAction of
    Nothing -> doInterface -- todo: Precisely what to do here?
    Just a -> case a of
                North -> bump (adjustPos N (snd charPos))
                Northwest -> bump (adjustPos NW (snd charPos))
                Northeast -> bump (adjustPos NE (snd charPos))
                South -> bump (adjustPos S (snd charPos))
                Southwest -> bump (adjustPos SW (snd charPos))
                Southeast -> bump (adjustPos SE (snd charPos))
                West -> bump (adjustPos W (snd charPos))
                East -> bump (adjustPos E (snd charPos))
                StayInPlace -> return Idle
                SaveGame -> do s <- gGlobal
                               mpr "{saving game}"
                               liftIO $ saveToFile "save" s
                               doInterface
                QuitGame -> return Quit

                UseFocusSlot s -> use s

use s = do
  po <- dereferObj playerCharRef
  case focus_ po !!/ s of
    Nothing -> doInterface
    Just fr -> do 
          fo <- dereferObj fr
          case fo of
            RangedWeapon { rangedWeaponPrototype_ = prot } -> do
                         myTargets <- pickShootingTargets
                         targetBeam <- target (def { targets = myTargets, 
                                                   acceptFun = notBlindly . notWhenBlocked tileBlocksVision $ anythingGoes })
                         case targetBeam of
                           Nothing -> doInterface
                           Just targetBeam' -> do
                                           singleTarget <- chooseAttackTarget (last targetBeam')
                                           case singleTarget of
                                             Nothing -> doInterface -- todo: what to do here?
                                             Just singleTarget' -> return $ Attack singleTarget' fr
            _ -> doInterface
{-              do myTargets <- pickShootingTargets
                 onM_ (target (def { targets = myTargets,
                                     acceptFun = notBlindly . notWhenBlocked tileBlocksVision $ anythingGoes })) $ \c ->
                        do onM_ (chooseAttackTarget (last c)) 
                                (playerCharRef `attack`)
-}

attack ra rd = do
  when (ra == playerCharRef) passTurn
  mGlobal (references ^: Map.adjust (health ^: subtract 1) rd)
  whenM ((<0) `liftM` health_ `liftM` dereferObj rd)
            $ removeActor rd

chooseAttackTarget c =
    do p <- gsGlobal positions_
       level <- playerLevel
       let rs = Set.elems $ Loc.lookupObjRefs (OnMap level c) p
       os <- mapM dereferObj rs
       fmap ref `fmap` (findM isAttackTargetable $ zipWith SpecificObject rs os)

{-  case keyToDirection c of 
    Just d -> bump (adjustPos d (snd charPos))
    Nothing -> 
        return Idle

-}
{-
        case c of
          KeyChar 'w' -> do s <- gGlobal
                            liftIO (writeFile "save" $ serialize s)
                            return ()
          KeyChar 'q' -> mGlobal ((doQuit ^= True) . (turnDone ^= True)) >> return ()
          KeyChar '5' -> passTurn
          KeyChar 'o' -> openDoor (snd charPos)
          KeyChar 'c' -> closeDoor (snd charPos)
          KeyChar ',' -> 
              do pickupM <- firstPickupInTile playerCharRef (snd charPos)
                 case pickupM of
                   Nothing -> return ()
                   Just p -> removeActor p
                 passTurn
                 return ()
                             
          KeyChar 'f' ->
              do myTargets <- pickShootingTargets
                 onM_ (target (def { targets = myTargets,
                                     acceptFun = notBlindly . notWhenBlocked tileBlocksVision $ anythingGoes })) $ \c ->
                        do onM_ (chooseAttackTarget (last c)) 
                                (playerCharRef `attack`)
          KeyChar 'j' -> do
                          t <- target (def { beamFun = jumpBeam,
                                             acceptFun = notBlindly . notWhenBlocked tileBlocksVision $ anythingGoes})
                          case t of
                            Nothing -> return ()
                            Just path -> tryJumpTo (SpecificObject playerCharRef o)  path
          KeyChar 'k' -> uncurry causeExplosion charPos
          _ -> do 
            return () -- xxx: complain about mispresses?
-}

bump :: (Monad m, MonadIO m, Functor m) => Coord -> GameT m Action
bump c = do
  level <- playerLevel
  rsos <- getPosObjs (OnMap level c)
  target <- findM isAttackTargetable rsos 
  o <- dereferObj playerCharRef
  case target of
    Just x -> return Idle
    Nothing -> do r <- try $ tryWalkTo (SpecificObject playerCharRef o) (level, c) 
                  case r of
                    Nothing -> return $ WalkTo c
                    Just msg -> mpr msg >> doInterface
-- $ return $ WalkTo c
    
{-
bump c = do
  level <- playerLevel
  rsos <- getPosObjs (OnMap level c)
  target <- findM isAttackTargetable rsos 
  o <- dereferObj playerCharRef
  case target of
    Just x -> attack playerCharRef (ref x) >> passTurn
    Nothing -> case find ((\o -> isDoor o && closed_ o) . obj) rsos of
                 Just (SpecificObject r o) -> touch r ((closed ^= False) $ o) >> passTurn
                 Nothing -> whenM (isNothing `fmap` tryWalkTo (SpecificObject playerCharRef o) (level, c)) passTurn

-}

-- todo: will need to be expanded into code that:
-- - damages items
-- - adds a random factor to item spread
-- - causes items to collide
-- - spreads kinetic energy in collisions
-- - records flings of all that
--  * we should be fine pretending that fling transfers form a tree. Thus, we just need to keep making a negative flingedProgress_ more negative as we go deeper to make fling sequences look natural. I'm not going to bother trying to actually simulate things so that objects can bump into other objects that have already been moved by the fling transfers, that would force me to actually add time and stuff. And would be far better implemented by having an actual in-flight state that objects could be during turns.

causeExplosion l c = do
  bs <- bounds `fmap` gsPlayerLevel world_
  adj <- adjacentsInWorldBounds l c
  let dirs = catMaybes $ map (uncurry (liftM2 (,))) $ map (directionOfAdjacentCoord c &&& Just) adj
  mapM_ (\(dir, coord) -> do
           objs <- getPosObjs (OnMap l coord)
           path <- takeWhileM (tileEnterable . (l,)) $ drop 1 $ iterate (adjustPos dir) coord
           case path of
             [] -> return ()
             _ -> mapM_ (\o -> do moveObject o (OnMap l $ last path)
                                  mPlayerLevel (flinged ^: (Flinged (ref o) path 0 :))) objs) dirs

anythingGoes = const $ return True

notBlindly :: (Monad m, Functor m) => ([Coord] -> GameT m Bool) -> [Coord] -> GameT m Bool
notBlindly f xs = 
    do v <- gsPlayerLevel playerVisible_
       other <- f xs
       return $ (other && 
                 case filter (flip Set.notMember v) xs of
                   [] -> True
                   _ -> False)


notWhenBlocked :: (Functor m, Monad m) => (Tile -> Bool) -> ([Coord] -> GameT m Bool) -> [Coord] -> GameT m Bool
notWhenBlocked blocks f xs =
    do other <- f xs
       w <- gsPlayerLevel world_
       return $ (other &&
                 none (blocks . lookupTile w) xs)

pickShootingTargets = do
  visible <- gsPlayerLevel playerVisible_
  level <- playerLevel
  filterM (\c -> do os <- getPosObjs (OnMap level c)
                    anyM (\e -> (not (ref e == playerCharRef) &&) `fmap` isAttackTargetable e) os)
              $ Set.toList visible



toggleDoor closing charPos = do
  let selectMsg = " of door to " ++ if closing then "close" else "open"
  level <- playerLevel
  adjs <- adjacentsInWorldBounds level charPos
  rsos <- getManyPosObjs (map (OnMap level) adjs)
  let doors = filter ((/=closing) . closed_ . obj) . 
              filter (isDoor . obj) 
                         $ rsos
              
  case doors of
    [] -> return ()
    ((SpecificObject r o):[]) -> touch r ((closed ^= closing) $ o) >> passTurn
    xs -> do onM_ (mapM (mapPosition . ref) xs 
                            >>= (selectDirection selectMsg . 
                                 sort . 
                                 mapMaybe (directionOfAdjacentCoord charPos .
                                 snd)))
                  (\dir -> let doorPos = adjustPos dir charPos in 
                           do (SpecificObject r o) <- head `fmap` 
                                                      filter (isDoor . obj) `fmap` 
                                                      getPosObjs (OnMap level doorPos)
                              touch r ((closed ^= closing) $ o)
                              passTurn)

openDoor = toggleDoor False
closeDoor = toggleDoor True

tryJumpTo r path = do
    level <- playerLevel
    ifM (or `fmap` (mapM tileEnterable $ map (level,) path))
        (moveObject r (OnMap level $ last path) >> passTurn)
        (return ())

onM d a act = do
    r <- a
    case r of
      Just x -> act x
      Nothing -> return d

onM_ = onM ()


findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM f (x:xs) = do { b <- f x; if b then return (Just x) else findM f xs }



jumpBeam c1 c2 = do
  let beams = allConnectingLines c1 c2
  level <- playerLevel
  okBeams <- filterM ((and `fmap`) . 
                      mapM tileEnterable) (map (map (level,) . drop 1) beams)
  return $ case okBeams of
             [] -> []
             (x:_) -> map snd x

shootingBeam c1 c2 =
    let beams = allConnectingLines c1 c2 in
    do do w <- gsPlayerLevel world_
          let okBeams = filter (none (tileBlocksMovement . (w!))) 
                               (map (drop 1) beams)
          return $ case okBeams of
                     [] -> []
                     (x:_) -> x

renderBar :: MonadIO m => (Int, Int) -> Int -> Int -> Int -> m ()
renderBar screenPos len maxFill fill = do
  let screenFilledLen = (fill * len) `div` maxFill
  liftIO $ uncurry mvAddStr screenPos ((replicate screenFilledLen '=') ++ 
                                       (replicate (len - screenFilledLen) '-'))
  return ()

firstAdjacentDoor c =
    do p <- gsGlobal positions_
       level <- playerLevel
       os <- mapM dereferObj $ concat [Set.elems $ Loc.lookupObjRefs (OnMap level nc) p |
                                       d <- [minBound..maxBound], nc <- [adjustPos d c]]
       let doors = filter isDoor os
       return $ maybeHead doors

firstPickupInTile r c =
    do p <- gsGlobal positions_
       level <- playerLevel
       let e = Set.elems $ Set.delete r $ Loc.lookupObjRefs (OnMap level c) p
       return $ maybeHead e

