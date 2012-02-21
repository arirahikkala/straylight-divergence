{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, TupleSections, NamedFieldPuns, ParallelListComp, DeriveDataTypeable #-}
module Main (main) where 

import RenderToHtml
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Array.IArray (amap, array, bounds)
import Data.Array.Unboxed

import Control.Monad

import Data.List (intercalate)
import Data.Maybe
import Data.Monoid

import Game
import Util
import BasicTypes
import CommonTypes
import Object
import Tile
import qualified LocationMap as Loc

import SaveLoad
import DataFile

import MainLoop

import Mapgen.Campus
import Mapgen.CampusArgs
import Mapgen.Furniture (indexPrototypes)

import UI.HSCurses.Curses hiding (echo, endWin, stdScr, noDelay)
import CursesWrap

import Control.Monad.Trans

import System.Random
import System.Environment (getArgs)

import Data.Accessor
import Control.Concurrent (threadDelay)
import DataUtil (amapAny)
import Compound

import Control.Monad.Reader (Reader, runReader)

import System.Console.GetOpt

introduceActor r = return ()


introduceObj (o, loc) =
    do r <- supplyId
       level <- playerLevel
       mGlobal (positions ^: Loc.introduce r (OnMap level loc))
       touch r o

lookupWeaponPrototype prot = Map.lookup prot `fmap` asksGlobal rangedWeaponPrototypes_

--prepMain :: (MonadGame m, Functor m, MonadIO m, MonadCurses m) => m ()
prepMain newGame = do
  mapOutput <- liftIO $ openFile "map.html" WriteMode
  when newGame $ do (w, doorsAndStuff, c) <- makeMap (CampusArgs 1 2 4 0.4 0.02) (Coord 0 0, Coord 50 50)
                    introduceActor playerCharRef
                    mPlayerLevel (world ^= w)
                    mPlayerLevel (compounds ^= makeCompounds (bounds w) c)
                    mPlayerLevel (playerExplored ^= amapAny (const False) w)
                    mPlayerLevel (playerRemembered ^= amapAny (const ' ') w)
                    replicateM 4 (dropObjectRandomly buddy)
                    mapM_ introduceActor [ObjRef 2 .. ObjRef 5]
                    mapM_ introduceObj doorsAndStuff
                    r <- supplyId
                    mGlobal (positions ^: Loc.introduce r (InContainer playerCharRef))
                    gunprot <- lookupWeaponPrototype "testgun"
                    case gunprot of
                      Nothing -> return ()
                      Just gunprot' -> touch r $ RangedWeapon gunprot'
                    po <- dereferObj playerCharRef
                    touch playerCharRef $ (focus ^= [r]) $ po
                    renderLevelToHtml (LevelRef 0) mapOutput

  liftIO $ hClose mapOutput

  s <- gGlobal
  randGen <- liftIO getStdGen

  liftIO $ do initCurses
              echo False
              startColor
              initColors
              withColor (Grey, Black) $ runGameT mainLoop randGen s defaultGameConfig
  return ()

-- the reason for using all these rather odd initial values (that will get replaced anyway), rather than undefineds, is that I'm using initialLevel as a (sanity) testing value..
initialLevel = 
    Level { world_ = listArray (Coord 0 0, Coord 0 0) [Tile 0]
          , playerExplored_ = listArray (Coord 0 0, Coord 0 0) [True]
          , playerVisible_ = Set.empty
          , playerRemembered_ = listArray (Coord 0 0, Coord 0 0) ['c']
          , flinged_ = []
          , compounds_ = Compounds (listArray (Coord 0 0, Coord 0 0) [CompoundRef 0]) (Map.empty)
          }

defaultGameState = 
    GameState { levels_ = Map.singleton (LevelRef 0) initialLevel
              , positions_ = Loc.LocationMap (Map.singleton playerCharRef (OnMap (LevelRef 0) (Coord 1 1))) (Map.singleton (OnMap (LevelRef 0) (Coord 1 1)) (Set.singleton playerCharRef))
              , references_ = Map.singleton playerCharRef defaultPlayer
              , firstUnusedId_ = ObjRef 2 -- make space for playerCharRef, at least for now
              , turnDone_ = False
              , doQuit_ = False
              , currentTurn_ = 0
              , messages_ = []
              }

dropObjectRandomly x = do
  w <- gsPlayerLevel world_
  level <- playerLevel
  c <- liftIO $ randomRIO (bounds w)
  e <- tileEnterable (level, c)
  if e
    then do r <- supplyId
            mGlobal (positions ^: Loc.insert r (OnMap level c))
            touch r x
    else dropObjectRandomly x

defaultGameConfig = 
    GameConfig [(KeyChar '1', Southwest), 
                (KeyChar '2', South), 
                (KeyChar '3', Southeast), 
                (KeyChar '4', West), 
                (KeyChar '5', StayInPlace), 
                (KeyChar '6', East), 
                (KeyChar '7', Northwest), 
                (KeyChar '8', North), 
                (KeyChar '9', Northeast), 
                (KeyA1, Northwest), 
                (KeyA3, Northeast), 
                (KeyC1, Southwest), 
                (KeyC3, Southeast), 
                (KeyDown, South), 
                (KeyUp, North), 
                (KeyLeft, West), 
                (KeyRight, East),
                (KeyChar 'q', QuitGame),
                (KeyChar 's', SaveGame),
                (KeyChar 'w', UseFocusSlot 0)] 
                   (Map.empty)
                   (Map.singleton "testgun" $ RangedWeaponPrototype "testiase" 2)

configureGame :: IO GameConfig
configureGame = 
    do prototypes <- liftIO ((read :: String -> [FurniturePrototype]) `fmap` readDataFile "FurniturePrototypes")
       return $ (defaultGameConfig { furniturePrototypes_ = indexPrototypes prototypes } )

-- todo: Really all Flag constructors should have only a string argument (so that their format can actually be properly checked rather than just being read-ed)
data Flag = LoadGame String | RandomSeed Int deriving Show

commandLineOptions = 
    [Option ['l'] ["loadgame"] (ReqArg LoadGame "start in main menu") "load a saved game",
     Option ['r'] ["randomseed"] (ReqArg (RandomSeed . read) "get from environment") "set random seed"]

main = do
  randGen1 <- getStdGen -- the standard generator is automatically initialized, but we don't know yet if we want to use it
  args <- getArgs
  let (opts, _, _) = getOpt Permute commandLineOptions args
      seed :: Int
      seed = fromMaybe (fst $ random randGen1) $ listToMaybe [s | RandomSeed s <- opts]
      randGen = mkStdGen seed
      saveGameSource = listToMaybe [s | LoadGame s <- opts]

  case saveGameSource of
    Nothing -> do config <- configureGame
                  runGameT (prepMain True) randGen defaultGameState config
                  return ()
    Just x -> do gameState <- loadGameFromFile x
                 case gameState of
                   Left e -> putStrLn ("error loading savefile " ++ x ++ ": " ++ e)
                   Right state -> do
                                  runGameT (prepMain False) randGen state defaultGameConfig
                                  return ()
  endWin

-- | Load a game config and run a GameConfig action with it
runWithDefaultConfig :: Reader GameConfig a -> IO a
runWithDefaultConfig a = 
    do config <- configureGame
       return $ runReader a config 

deserialize = error "implement deserialize"