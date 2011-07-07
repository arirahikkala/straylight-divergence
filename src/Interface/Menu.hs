{-# LANGUAGE ParallelListComp, TemplateHaskell, TupleSections, TypeFamilies, UndecidableInstances, StandaloneDeriving #-}

module Interface.Menu where

import Control.Arrow hiding (first, second)
import qualified Control.Arrow as Arrow (first, second)
import BasicTypes
import CommonTypes
import Util (unpick, pick, matching, listIndex, takeUntilLimit, foldPart, applyOnIndex)
import Data.List (groupBy, findIndex, minimumBy, find, mapAccumL)
import Control.Monad (foldM, when, void)
import Control.Monad.Trans (liftIO)
import CursesWrap
import qualified UI.HSCurses.Curses as C
import UI.HSCurses.Curses (Key(..))
import Data.Ord (comparing)
import Data.VectorSpace ((^+^))
import Text

import Data.Accessor
import Data.Accessor.Tuple
import Data.Accessor.Template

deriving instance Read Key

data Menu a = Menu {
      upperLeft_ :: (Int, Int)
    , sizeBound_ :: (Int, Int)
    , args_ :: MenuArgs
    -- which page we're on, and which item on that page we're on
    , cursorPosition_ :: (Int, Int)
    -- to keep the cursor from "drifting" while paging through a list with non-uniform-length items
    , preferredScreenPosition_ :: Int
    , numSelected_ :: Int
    , pages_ :: [[MenuItem a]]
} deriving (Show)

data MenuItem a = MenuItem {
      value_ :: Maybe a
    , color_ :: ColorName -- do we want fancier color support (i.e. can set the entire style? Nnnnaaaaaaaaahh, we don't)
    , text_ :: [TextLine]
    , quantity_ :: Int
    , selectedQuantity_ :: Int
    , accelerators_ :: [Char]
} deriving (Show)

items = concat . pages_
selectedItems = filter ((>0) . selectedQuantity_) . items

simpleStringItem :: String -> MenuItem String
simpleStringItem s =
    MenuItem (Just s) Green (lines s) 1 0 [head s]

type TextLine = String

data MenuArgs = MenuArgs {
      showAccelerators_ :: Bool
    , useCursor_ :: Bool
    , allowPartialSelection_ :: Bool
} deriving (Show)

instance Def MenuArgs where
    def = MenuArgs {
            showAccelerators_ = True
          , useCursor_ = False
          , allowPartialSelection_ = False
}

$(deriveAccessors ''MenuItem)
$(deriveAccessors ''Menu)

currentPage :: Accessor (Menu a) [MenuItem a]
currentPage = accessor (\m -> pages_ m !! fst (cursorPosition_ m))
                       (\x m -> ((pages .> listIndex (fst (cursorPosition_ m))) ^= x) $ m)

currentPage_ m = pages_ m !! fst (cursorPosition_ m)

grabItem :: (MenuItem a -> Bool) -> Menu a -> Maybe (Accessor (Menu a) (MenuItem a))
grabItem p m =
    grabItem' (pages_ m) 0 where
        grabItem' [] n = Nothing
        grabItem' (x:xs) n = 
            case findIndex p x of
              Nothing -> grabItem' xs (n + 1)
              Just i -> Just (pages .> listIndex n .> listIndex i)

menuProcessKey m k = do
  case menuProcessAccelerator m k of
    Nothing -> do r <- menuProcessBoundKey m k
                  case r of
                    Nothing -> return (False, m)
                    (Just newMenu) -> return (True, newMenu)
    (Just newMenu) -> return (True, newMenu)


menuProcessAccelerator :: Menu a -> Key -> Maybe (Menu a)
menuProcessAccelerator m k = do
  case k of 
    KeyChar c -> 
        let ((newNumSelected, changed), newPages) = 
                mapAccumL (mapAccumL (\(ns, nc) item -> 
                                          case (elem c (accelerators_ item), quantity_ item > 0, selectedQuantity_ item > 0) of
                                            (False, _, _) -> ((ns, nc), item)
                                            (True, False, _) -> ((ns, nc), item)
                                            (True, True, False) -> ((succ ns, succ nc), (selectedQuantity ^= quantity_ item) $ item)
                                            (True, True, True) -> ((pred ns, succ nc), (selectedQuantity ^= 0) $ item)))
                          (numSelected_ m, 0)
                          (pages_ m)
        in if changed == 0
           then Nothing
           else Just $ ((numSelected ^= newNumSelected) . (pages ^= newPages)) $ m
    _ -> Nothing
recordPreferredPosition :: Menu a -> Menu a
recordPreferredPosition m = (preferredScreenPosition ^= (length $ concat $ take (snd (cursorPosition_ m)) $ map text_ $ currentPage_ m)) $ m

data MenuMove = MovePage Int | MoveIndex Int
-- todo: break this into two functions, MenuMove really has no reason to exist

menuMoveCursor :: Menu a -> MenuMove -> Maybe (Menu a)
menuMoveCursor m (MovePage n) 
    | fst newPos < 0 = Just $ recordPreferredPosition $ (cursorPosition ^= (0, 0)) $ m
    | fst newPos >= length (pages_ m) = Just $ recordPreferredPosition $ (cursorPosition ^= (length (pages_ m) - 1, length (last $ pages_ m) - 1)) $ m
    | otherwise = Just $
                  (cursorPosition ^= 
                   (fst newPos, 
                    closestItemOnPage (fst newPos)
                                      m 
                                      (preferredScreenPosition_ m))) $ m
      where newPos = (first ^: (+ n)) $ cursorPosition_ m

menuMoveCursor m (MoveIndex n)
    | not (useCursor_ $ args_ m) = Nothing
    | fst newPos < 0 = Just $ recordPreferredPosition $ (cursorPosition ^= (0, 0)) $ m
    | fst newPos >= length (pages_ m) 
      || (fst newPos >= (length (pages_ m) - 1) && snd newPos >= (length (last $ pages_ m) - 1))
          = Just $ recordPreferredPosition $ (cursorPosition ^= (length (pages_ m) - 1, length (last $ pages_ m) - 1)) $ m
    | snd newPos < 0 || snd newPos >= length (currentPage_ m)
        = let oldFullIndex = firstItemOnScreen m + (snd $ cursorPosition_ m)
              ((pagesBeforeNewPage, _), numItemsBeforeNewPage) =  
                  takeUntilLimit length
                                 (oldFullIndex + n)
                                 (pages_ m) in
          Just $ (cursorPosition ^= (length pagesBeforeNewPage, max 0 (oldFullIndex + n - numItemsBeforeNewPage))) $ m
    | otherwise = Just $ (cursorPosition ^= newPos) $ m
    where newPos = (second ^: (+ n)) $ cursorPosition_ m


menuProcessBoundKey :: (Monad m, Functor m) => Menu a -> Key -> GameT m (Maybe (Menu a))
menuProcessBoundKey m k =
    do ma <- getKeyAction k
       case ma of
         Nothing -> return Nothing

         Just a -> return $
                   case a of
                     MenuPrevious -> 
                         menuMoveCursor m (MoveIndex (negate 1))
                     MenuNext -> 
                         menuMoveCursor m (MoveIndex 1)
                     MenuPrevPage ->
                         menuMoveCursor m (MovePage (negate 1))
                     MenuNextPage ->
                         menuMoveCursor m (MovePage 1)
                     _ -> Nothing

getKeyAction :: Monad m => Key -> GameT m (Maybe KeyAction)
getKeyAction k = 
    do bindings <- asksGlobal keyBindings
       return $ lookup k bindings

firstItemOnScreen m = sum (map length $ take (fst $ cursorPosition_ m) $ pages_ m)
lastItemOnScreen m = firstItemOnScreen m + pred (length $ currentPage_ m)

-- which item on the given page starts closest to the given starting line?
closestItemOnPage p m l =
    fst . 
    minimumBy (comparing (abs . (l -) . snd)) . 
    zip [0..] .
    scanl (+) 0 . 
    map (length . text_) $ 
    (pages_ m !! p)

clamp v low high = max low (min v high)

renderMenu m = liftIO $ do 
  let accelx = fst $ upperLeft_ m
      itemx = accelx + if showAccelerators_ $ args_ m then 4 else 0
  clearArea (upperLeft_ m) ((upperLeft_ m) ^+^ (sizeBound_ m))
  foldM (\(height, index) item -> withFg (color_ item) $ do 
           (case ((useCursor_ $ args_ m) && index == snd (cursorPosition_ m), 
                  selectedQuantity_ item > 0,
                  quantity_ item > 0) of
              (_, _, False) -> withStyle (Style True False Black Black)
              (True, _, _) -> withColor (Black, Grey)
              (False, True, _) -> withBold True
              _ -> id) $ do
             clearArea (accelx, height) (accelx + fst (sizeBound_ m), height + length (text_ item) - 1)
             when (showAccelerators_ $ args_ m) $ 
                  void $ mvAddStr accelx height $
                           ((head (accelerators_ item)) : 
                            if selectedQuantity_ item > 0 
                            then " + "
                            else " - ")
             nextHeight <- foldM (\iheight line ->
                                      (mvAddStr itemx iheight line >> return (succ iheight)))
                                 height
                                 (text_ item)
             return (nextHeight, index + 1))
        (snd $ upperLeft_ m, 0)
        (currentPage_ m)
  return ()

menu upperLeft sizeBound menuArgs items =
    recordPreferredPosition $ 
    Menu upperLeft
         sizeBound
         menuArgs
         (0, 0)
         0
         (length $ filter ((>0) . selectedQuantity_) $ items)
         (foldPart (page (snd sizeBound)) $ items)



testMenu1 = menu (0, 0) (40, 40) def $ map simpleStringItem ["hello\nyou guys", "there", "world"]
testMenu2 = menu (0, 0) (39, 39) (def { useCursor_ = False, showAccelerators_ = True } ) $ concat $ replicate 400 $ applyOnIndex 2 (quantity ^= 0) $ map simpleStringItem ["hello\nyou guys", "there", "world"]

page :: Int -> [MenuItem a] -> ([MenuItem a], [MenuItem a])
page height [] = ([], [])
page height xs =
    case fst $ takeUntilLimit (length . text_) height xs
    of ([], _) -> ([head xs], tail xs) -- couldn't fit even a single item in? Just mash it in so we can display *something*
       r -> r
