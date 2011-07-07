module Main where

import Interface.Menu
import qualified UI.HSCurses.Curses as C
import BasicTypes
import CommonTypes
import CursesWrap
import UI.HSCurses.Curses (Key (..))
import Control.Monad.Trans (liftIO)


doMenu menu = do
  renderMenu menu
  liftIO refresh
  c <- liftIO getCh
  r <- menuProcessKey menu c
  case r of 
    (True, newMenu) -> liftIO (writeFile "foo" $ show newMenu) >> doMenu newMenu
    (False, _) -> return ()

main = do 
  C.initCurses
  C.startColor
  C.keypad stdScr True
  initColors
  erase
  runGameT (doMenu testMenu2) undefined undefined (GameConfig [(KeyChar 'n', MenuNext), 
                                                               (KeyChar 'p', MenuPrevious), 
                                                               (KeyNPage, MenuNextPage),
                                                               (KeyPPage, MenuPrevPage)])
  endWin
