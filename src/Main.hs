{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, TupleSections, NamedFieldPuns, ParallelListComp #-}
module Main (main) where 

import RenderToHtml
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Array.IArray (amap, array, bounds)
import Data.Array.Unboxed

import Control.Monad

import Data.List (intercalate)

import Game
import Util
import BasicTypes
import CommonTypes
import Object
import Tile
import qualified LocationMap as Loc

import SaveLoad
import Interface

import Mapgen.Campus
import Mapgen.CampusArgs

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

introduceActor r = return ()


introduceObj (o, loc) =
    do r <- supplyId
       level <- playerLevel
       mGlobal (positions ^: Loc.introduce r (OnMap level loc))
       touch r o

--prepMain :: (MonadGame m, Functor m, MonadIO m, MonadCurses m) => m ()
prepMain newGame = do
  mapOutput <- liftIO $ openFile "map.html" WriteMode
  when newGame $ do (w, doorsAndStuff, c) <- makeMap (CampusArgs 1 2 4 0.4 0.02) (Coord 0 0, Coord 50 50)
--                    liftIO $ threadDelay 1000000
                    introduceActor playerCharRef
                    mPlayerLevel (world ^= w)
                    mPlayerLevel (compounds ^= makeCompounds (bounds w) c)
                    mPlayerLevel (playerExplored ^= amapAny (const False) w)
                    mPlayerLevel (playerRemembered ^= amapAny (const ' ') w)
                    replicateM 4 (dropObjectRandomly buddy)
                    mapM_ introduceActor [ObjRef 2 .. ObjRef 5]
                    mapM_ introduceObj doorsAndStuff
                    renderLevelToHtml (LevelRef 0) mapOutput

  liftIO $ hClose mapOutput

  s <- gGlobal
  randGen <- liftIO getStdGen

  liftIO $ do initCurses
              echo False
              startColor
              initColors
              withColor (Grey, Black) $ startMainLoop randGen s defaultGameConfig
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

main = playGame

defaultGameConfig = GameConfig [] (Map.empty)

playGame = do
  randGen <- liftIO getStdGen
  args <- getArgs
  case args of
    [] -> runGameT (prepMain True) randGen defaultGameState defaultGameConfig
    (x:_) -> do saveContent <- readFile x
                gameState <- runWithDefaultConfig ((deserialize :: StoredState -> Reader GameConfig GameState) $ read saveContent)
                runGameT (prepMain False) randGen gameState defaultGameConfig
  endWin

-- | Load a game config and run a GameConfig action with it
runWithDefaultConfig :: Reader GameConfig a -> IO a
runWithDefaultConfig = return . flip runReader defaultGameConfig 