{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DisambiguateRecordFields, NamedFieldPuns, StandaloneDeriving, FlexibleContexts, UndecidableInstances, NoMonomorphismRestriction, FlexibleInstances, BangPatterns, DeriveDataTypeable #-}
module CommonTypes where
import Object
import BasicTypes

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)

import LocationMap
--import qualified LocationMap as Loc

import Data.Int (Int64)

import Data.Accessor ((^:), (^=))
import Data.Accessor.Template

import System.Random

import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.State.Class (get, put)

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask, asks)

import Control.Monad.Random (runRandT, RandT, MonadRandom)
import Control.Monad.Random.Class (getRandom, getRandoms, getRandomR, getRandomRs)
import Control.Monad.Trans (lift, MonadIO)

import Control.Arrow (first)
import Data.Data
import Data.Typeable

import Tile

import DataUtil (copyArray)

import UI.HSCurses.Curses (Key (..))

import Compound

import Data.DeriveTH
import Data.Derive.NFData
import Control.DeepSeq

data Facing = N | NE | E | SE | S | SW | W | NW
                 deriving (Eq, Ord, Show, Read, Enum, Bounded)


newtype GameT m a = GameT { runGame :: ReaderT GameConfig (StateT GameState (RandT StdGen m)) a }
    deriving (Monad, Functor, MonadIO)

gGlobal = GameT get
pGlobal = GameT . put
askGlobal = GameT ask
asksGlobal = GameT . asks

runGameT (GameT a) g s c = 
    runRandT (runStateT (runReaderT a c) s) g

--evalGameT a g s = 
--    fst `fmap` fst `fmap` runGameT a g s

instance Monad m => MonadRandom (GameT m) where
    getRandom = GameT $ lift $ lift getRandom
    getRandoms = GameT $ lift $ lift getRandoms
    getRandomR = GameT . lift . lift . getRandomR
    getRandomRs = GameT . lift . lift . getRandomRs

type Levels = Map LevelRef Level
type World = Array Coord Tile
type References = Map ObjRef Object
type Positions = LocationMap ObjRef Location
type Remembered = UArray Coord Char
type Explored = UArray Coord Bool
type Visible = Set Coord

-- Dunno why this instance or something like it doesn't exist by default
instance (Ix i, Read i, Read e, IArray UArray e) 
    => Read (UArray i e) where
    readsPrec precedence input = 
        map (first copyArray) $ 
        (readsPrec :: (Ix i, Read i, Read e) => Int -> String -> [(Array i e, String)]) precedence input

data GameConfig = GameConfig {
      keyBindings_ :: [(Key, KeyAction)]
    , furniturePrototypes_ :: Map String FurniturePrototype
}

data KeyAction = North | Northwest | West | Southwest | South | Southeast | East | Northeast | MenuPrevious | MenuNext | MenuPrevPage | MenuNextPage | MenuSelect | MenuFinish
                 deriving (Show, Read)
{-
  One of the annoying downsides of Haskell is that it can be difficult to track
  down space leaks. The fortunate thing is that at *least* you know everything
  that's leaked for longer than a turn must have a reference from 
  GameState - all other stuff is either garbage collected or immutable.
  Therefore, it's enough to just ensure that all parts of GameState are
  treated in a way that prevents space leaks.

  Possible future direction: Un-export the GameState constructor and record
  accessors, force all mutation of GameState to go through an interface that
  ensures strictness.

-}

data GameState = GameState {
      levels_ :: Levels

    -- only mutated through strictness-ensuring interface LocationMap.hs
    , positions_ :: Positions

    -- only mutated through strictness-ensuring interface Game.touch
    , references_ :: References

    -- trivial
    , firstUnusedId_ :: ObjRef

    -- trivial
    , currentTurn_ :: Time
    -- emptied regularly
    , messages_ :: [String]

    -- trivial
    , turnDone_ :: Bool
    -- trivial
    , doQuit_ :: Bool
} deriving (Show, Read)

data Flinged = Flinged {
      flingedObject_ :: ObjRef
    , flingedPath_ :: [Coord]
    -- flingedProgress_ == maxBound is currently special, meaning the fling is "done"
    , flingedProgress_ :: Int
} deriving (Show, Read)

data Level = Level {
    -- currently never mutated
      world_ :: Array Coord Tile
    -- currently never mutated
    , compounds_ :: Compounds

    -- unboxed array
    , playerExplored_ :: !Explored
    -- emptied regularly
    , playerVisible_ :: !Visible
    -- unboxed array
    , playerRemembered_ :: !Remembered
    -- emptied regularly
    , flinged_ :: [Flinged] -- todo: this and possibly some other ones are really more generally interface-related data and should be made ephemeral
} deriving (Show, Read)

type Time = Int


$( deriveAccessors ''GameState )
$( deriveAccessors ''Level )
$( deriveAccessors ''Flinged )

mGlobal f = do s <- gGlobal
               pGlobal (f s)

gsGlobal f = do s <- gGlobal
                return $ f s

gLevel l = (Map.! l) `fmap` levels_ `fmap` gGlobal

gsLevel l f = f `fmap` gLevel l

mLevel l f = mGlobal (levels ^: Map.adjust f l) 

pLevel l x = mGlobal (levels ^: Map.insert l x)



instance Random Facing where
    random = (\(a, g) -> (toEnum a, g)) . randomR (fromEnum (minBound :: Facing), fromEnum (maxBound :: Facing))
    randomR (s, e) g = (\(a, g) -> (toEnum a, g)) $ randomR (fromEnum s, fromEnum e) g

instance Random Int64 where
    random = (\(a, g) -> (toEnum a, g)) . randomR (fromEnum (minBound :: Int64), fromEnum (maxBound :: Int64))
    randomR (s, e) g = (\(a, g) -> (toEnum a, g)) $ randomR (fromEnum s, fromEnum e) g
