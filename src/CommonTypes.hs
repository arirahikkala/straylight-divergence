{-# LANGUAGE GeneralizedNewtypeDeriving, DisambiguateRecordFields, NamedFieldPuns, StandaloneDeriving, FlexibleContexts, NoMonomorphismRestriction, FlexibleInstances, BangPatterns, MultiParamTypeClasses, TypeSynonymInstances, DeriveGeneric, TemplateHaskell, DeriveDataTypeable, OverlappingInstances #-}
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

import Control.Monad.State.Strict (StateT, runStateT, evalStateT)
import Control.Monad.State.Class (get, put)

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (ask, asks)

import Control.Monad.Random (runRandT, RandT, MonadRandom)
import Control.Monad.Random.Class (getRandom, getRandoms, getRandomR, getRandomRs)
import Control.Monad.Trans (lift, MonadIO)

import Control.Arrow (first)
import Data.Typeable
import Data.YamlObject (ToYaml, FromYaml, FromYamlM, fromYaml, TranslateField (..))
import qualified Data.Text as Text
import Tile

import DataUtil (copyArray)

import UI.HSCurses.Curses (Key (..))
import CursesWrap (StyledChar, Style, ColorName)
import Control.Monad.Identity
import Control.Monad.Random


import GHC.Generics (Generic)

data Facing = N | NE | E | SE | S | SW | W | NW
                 deriving (Eq, Ord, Show, Read, Enum, Bounded)


newtype GameT m a = GameT { runGame :: ReaderT GameConfig (StateT GameState (RandT StdGen m)) a }
    deriving (Monad, Functor, MonadIO)

instance Monad m => MonadSplit StdGen (GameT m) where
    getSplit = GameT (lift . lift $ getSplit)

type PureGame a = GameT Identity a

gGlobal = GameT get
pGlobal = GameT . put
askGlobal = GameT ask
asksGlobal = GameT . asks

runGameT (GameT a) g s c = 
    runRandT (runStateT (runReaderT a c) s) g

evalGameT a g s c = 
    fst `fmap` fst `fmap` runGameT a g s c

-- evalGameT, taking the game state and game config from the enclosing state, and using a static RNG. Basically, "do this thing, but just get the return value and ignore its changes to the game state"
try (GameT a) = do
  gamestate <- gGlobal
  gameconfig <- askGlobal
  evalRandT (evalStateT (runReaderT a gameconfig) gamestate) (mkStdGen 0)


instance Monad m => MonadRandom (GameT m) where
    getRandom = GameT $ lift $ lift getRandom
    getRandoms = GameT $ lift $ lift getRandoms
    getRandomR = GameT . lift . lift . getRandomR
    getRandomRs = GameT . lift . lift . getRandomRs

type Levels = Map LevelRef Level
type World = Array Coord Tile
type References = Map ObjRef Object
type Positions = LocationMap ObjRef Location
type Remembered = Array Coord Char
type Explored = Array Coord Bool
type Visible = Set Coord

type Initiative = Int

-- Dunno why this instance or something like it doesn't exist by default
-- todo: put somewhere else
instance (Ix i, Read i, Read e, IArray UArray e) 
    => Read (UArray i e) where
    readsPrec precedence input = 
        map (first copyArray) $ 
        (readsPrec :: (Ix i, Read i, Read e) => Int -> String -> [(Array i e, String)]) precedence input

data GameConfig = GameConfig {
      keyBindings_ :: [(Key, KeyAction)]
    , furniturePrototypes_ :: Map String FurniturePrototype
    , rangedWeaponPrototypes_ :: Map String RangedWeaponPrototype
}

data KeyAction = 
    North | Northwest | West | Southwest | South | Southeast | East | Northeast | StayInPlace |
    UseFocusSlot Int |
    MenuPrevious | MenuNext | MenuPrevPage | MenuNextPage | MenuSelect | MenuFinish |
    QuitGame | SaveGame
                 deriving (Show, Read, Eq)
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

    -- only mutated through strictness-ensuring interface defined in LocationMap.hs
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
} deriving (Show, Typeable, Generic)


data Flinged = Flinged {
      flingedObject_ :: ObjRef
    , flingedPath_ :: [Coord]
    -- flingedProgress_ == maxBound is currently special, meaning the fling is "done"
    , flingedProgress_ :: Int
} deriving (Show, Typeable, Generic)

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
} deriving (Show, Typeable, Generic)

type Time = Int

data IdlingType = UseWorkstation | GetStuffOnShelf deriving (Show, Read, Eq, Typeable, Generic)

data IdlingPoint = IdlingPoint {
      ipIdlingType_ :: IdlingType
    , ipLocation_ :: Coord
} deriving (Show, Read, Eq, Typeable, Generic)

instance TranslateField IdlingPoint where
    translateField _ = Text.init

newtype CompoundRef = CompoundRef Int deriving (Show, Read, Eq, Ord, Enum, Typeable, Generic)

data Compounds = Compounds {
      compoundsCoords_ :: Array Coord CompoundRef
    , compoundsRefs_ :: Map CompoundRef Compound
} deriving (Show, Read, Typeable, Generic)

data Compound = Compound {
      compoundRooms_ :: [BoundsRect]
    , idlingPoints_ :: [IdlingPoint]
} deriving (Show, Read, Typeable, Generic)

$( deriveAccessors ''GameState )
$( deriveAccessors ''GameConfig )
$( deriveAccessors ''Level )
$( deriveAccessors ''Flinged )
$( deriveAccessors ''IdlingPoint )

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



instance FromYaml GameState
instance FromYaml Level
instance FromYaml Object
instance FromYaml Tile
instance FromYaml Compounds
instance FromYaml Flinged
instance FromYaml AIState
instance FromYaml RangedWeaponPrototype
instance FromYaml FurniturePrototype
instance FromYaml RubbleMaterial
instance FromYaml Location
instance FromYaml StyledChar
instance FromYaml Style
instance FromYaml ColorName
instance FromYaml CompoundRef
instance FromYaml Compound
instance FromYaml IdlingPoint
instance FromYaml IdlingType

instance ToYaml GameState
instance ToYaml Level
instance ToYaml Object
instance ToYaml Tile
instance ToYaml Compounds
instance ToYaml Flinged
instance ToYaml AIState
instance ToYaml RangedWeaponPrototype
instance ToYaml FurniturePrototype
instance ToYaml RubbleMaterial
instance ToYaml Location
instance ToYaml StyledChar
instance ToYaml Style
instance ToYaml ColorName
instance ToYaml CompoundRef
instance ToYaml Compound
instance ToYaml IdlingPoint
instance ToYaml IdlingType

instance (Show ml, Show mr, Typeable ml, Typeable mr, Ord ml, Ord mr, ToYaml ml, ToYaml mr) => ToYaml (LocationMap ml mr)

instance (Read ml, Read mr, Typeable ml, Typeable mr, Ord ml, Ord mr, FromYaml ml, FromYaml mr) => FromYaml (LocationMap ml mr)