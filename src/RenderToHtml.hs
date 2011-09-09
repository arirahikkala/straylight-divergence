module RenderToHtml where

import System.IO
import Control.Monad.Trans (liftIO)
import CursesWrap
import Interface
import Data.Array
import BasicTypes
import CommonTypes

prelude = "<html><head><style>td {border: none; width: 1em; min-width: 1em; max-width: 1em}</style><title>htmlshot</title></head><body><table border=\"0\" cellspacing=\"0\" style=\"border: 0 0 0 0; margin: 0 0 0 0; \">"
postlude = "</table></body></html>"

color True name = 
    case name of
      Black -> "808080"
      Red -> "0000ff"
      Green -> "00ff00"
      Yellow -> "ffff00"
      Blue -> "ff0000"
      Purple -> "ff00ff"
      Cyan -> "00ffff"
      Grey -> "ffffff"

color False name =
    case name of
      Black -> "000000"
      Red -> "000088"
      Green -> "008800"
      Yellow -> "888800"
      Blue -> "880000"
      Purple -> "880088"
      Cyan -> "008888"
      Grey -> "c0c0c0"


renderCharToHtml (StyledChar (Style bright brightBg fgColor bgColor) c) =
    "<td style=\"color: #" ++
    color bright fgColor ++
    "; background-color: #" ++
    color brightBg bgColor ++
    ";\">" ++ [c] ++ "</td>"

renderCharToHtml (StyleAnimatedChar ((Style bright brightBg fgColor bgColor):_) c) =
    "<td style=\"color: #" ++
    color bright fgColor ++
    "; background-color: #" ++
    color brightBg bgColor ++
    ";\">" ++ [c] ++ "</td>"


renderLevelToHtml level outputHandle = do
  (Coord bx by, Coord ex ey) <- bounds `fmap` gsLevel level world_
  liftIO $ hPutStr outputHandle prelude
  flip mapM_ [by..ey] $ \line -> do 
--    liftIO $ print [OnMap level (Coord x line) | x <- [bx..ex]]
    chars <- mapM (renderPos True) $ [Coord x line | x <- [bx..ex]]
    liftIO $ hPutStr outputHandle ("<tr>" ++ concatMap renderCharToHtml chars ++ "</tr>")
  liftIO $ hPutStr outputHandle postlude
