{-# LANGUAGE ForeignFunctionInterface, NoMonomorphismRestriction, ParallelListComp, DeriveDataTypeable  #-}
module CursesWrap where

import Data.Data
import Data.Typeable
import Control.Arrow ((***))
import qualified UI.HSCurses.Curses as C
import qualified UI.HSCurses.CursesHelper as CH
import Foreign.C (CInt, CChar)
import Foreign.C.String
import Foreign (Ptr)

import Data.Monoid (Monoid, mappend, mempty)
-- wrap the curses functions that I use in order to give me back my nice x, y - ordered arguments rather than the confusing y, x - ordered ones, 
-- and to bring all the curses stuff in one place so I don't need both Curses and CursesHelper :p

noDelay = C.noDelay

timeout = C.timeout

castEnum = toEnum . fromEnum

mvAddCh :: Int -> Int -> Char -> IO ()
mvAddCh x y = C.mvAddCh y x . castEnum

move x y = C.move y x

getxy = (\(y, x) -> (x, y)) `fmap` C.getYX C.stdScr

addCh = C.waddch C.stdScr . castEnum

--scrSize = C.scrSize

wclear = C.wclear

refresh = C.refresh

getCh = C.getCh

--start = CH.start

--end = CH.end

initScr = C.initScr

echo = C.echo

nl = C.nl

--cursSet = C.cursSet

--newWin = C.newWin

--update = C.update

--clearOk = C.clearOk

--mvWAddStr = C.mvWAddStr

stdScr = C.stdScr

endWin = C.endWin

--wrefresh :: C.Window -> IO ()
--wrefresh w = C.throwIfErr_ "wrefresh" $ wrefresh_c w

--foreign import ccall unsafe "HSCurses.h wrefresh"
--	wrefresh_c :: C.Window -> IO CInt

foreign import ccall unsafe "HSCurses.h mvaddstr"
        mvaddstr_c :: CInt -> CInt -> Ptr CChar -> IO CInt

mvAddStr :: Int -> Int -> String -> IO Int
mvAddStr x y str =
    withCString str $ \s ->
        (toEnum . fromEnum) `fmap` mvaddstr_c (toEnum . fromEnum $ y) (toEnum . fromEnum $ x) s

foreign import ccall unsafe "HSCurses.h erase"
        erase_c :: IO CInt

erase :: IO Int
erase = (toEnum . fromEnum) `fmap` erase_c

foreign import ccall unsafe "HSCurses.h werase"
        werase_c :: C.Window -> IO CInt

werase :: C.Window -> IO Int
werase w = (toEnum . fromEnum) `fmap` werase_c w

foreign import ccall unsafe "HSCurses.h wgetch"
        wgetch_c :: C.Window -> IO CInt

wgetch w = C.decodeKey `fmap` wgetch_c w

clearArea (x1, y1) (x2, y2) =
    sequence_ [mvAddCh x y ' ' | x <- [x1..x2], y <- [y1..y2]]

data ColorName = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Typeable)

getPair :: (ColorName, ColorName) -> C.Pair
getPair (fg, bg) = C.Pair (1 + ((8 * fromEnum bg) + (fromEnum fg)))

getColorNames :: C.Pair -> (ColorName, ColorName)
getColorNames (C.Pair 0) = (Grey, Black)
getColorNames (C.Pair n) = (\(bg, fg) -> (toEnum fg, toEnum bg)) $ divMod (n - 1) 8 

initColors = mapM_ (\(x, y, z) -> C.initPair x y z) 
             [(C.Pair n, C.Color f, C.Color b) |
              n <- [1..] |
              b <- [0..7],
              f <- [0..7]]


instance Monoid CH.ForegroundColor where
    mempty = CH.DefaultF
    mappend CH.DefaultF x = x
    mappend x _ = x

instance Monoid CH.BackgroundColor where
    mempty = CH.DefaultB
    mappend CH.DefaultB x = x
    mappend x _ = x

withStyle (Style bright brightBg fgColor bgColor) a = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (flip C.setBlink brightBg (C.setBold C.attr0 bright)) $ getPair (fgColor, bgColor)
  r <- a
  uncurry C.attrSet old
  return r

withColor c a = do
  old <- C.wAttrGet C.stdScr
  setColor c
  r <- a
  uncurry C.attrSet old
  return r

setStyle (Style bright brightBg fgColor bgColor) = 
    C.attrSet (flip C.setBlink brightBg (C.setBold C.attr0 bright)) $ getPair (fgColor, bgColor)

setColor = C.attrSet C.attr0 . getPair

setFg c = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (fst old) $ getPair (c, (snd $ getColorNames $ snd old))

setBg c = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (fst old) $ getPair ((fst $ getColorNames $ snd old), c)

withFg c a = do
  old <- C.wAttrGet C.stdScr
  setFg c
  r <- a
  uncurry C.attrSet old
  return r

withBg c a = do
  old <- C.wAttrGet C.stdScr
  setBg c
  r <- a
  uncurry C.attrSet old
  return r

setBold b = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (C.setBold (fst old) b) (snd old)

setBoldBg b = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (C.setBlink (fst old) b) (snd old)

withBold b a = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (C.setBold C.attr0 b) $ snd old
  r <- a
  uncurry C.attrSet old
  return r

withBoldBg b a = do
  old <- C.wAttrGet C.stdScr
  C.attrSet (C.setBlink C.attr0 b) $ snd old
  r <- a
  uncurry C.attrSet old
  return r

data Update a = New a | Revert | None

data UpdateStyle = UpdateStyle {
      updateBright :: Update Bool
    , updateBrightBg :: Update Bool
    , updateFgColor :: Update ColorName
    , updateBgColor :: ColorName
}

data Style = Style {
      bright :: Bool
    , brightBg :: Bool
    , fgColor :: ColorName
    , bgColor :: ColorName
} deriving (Show, Read, Data, Typeable)

data StyledChar = StyledChar {
      style :: Style
    , c :: Char 
    } deriving (Show, Read, Data, Typeable)
