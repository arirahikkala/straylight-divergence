module Text where

import Util (takeUntilLimit, foldPart)
import CursesWrap
import BasicTypes
import CommonTypes
import Control.Monad.State
import Control.Arrow

import Data.List --(intercalate, tails)

import Data.IORef

data TextOp = TextString String | TextFgColor ColorName | TextBgColor ColorName | TextBold Bool | TextBoldBg Bool | Pop
            deriving Show
type FormattedText = [TextOp]

renderAt x y xs = do
  move x y
  render xs

render xs = do
  start <- getxy
  render' start [] xs

printedLength = length -- todo: implement

render' _ _ [] = return ()
render' start [] (Pop:xs) = setStyle (Style True True Green Red) >> render' start [] xs -- should be loud enough to get any text bugs fixed :P
render' start (s:stack) (Pop:xs) = 
    (case s of
      TextFgColor c -> setFg c
      TextBgColor c -> setBg c
      TextBold b -> setBold b
      TextBoldBg b -> setBoldBg b
      _ -> return ()) >> render' start stack xs
render' start stack ((TextString t):xs) = do lineNow <- renderLines start t; render' (fst start, lineNow) stack xs
render' start stack (op:xs) = render' start (op:stack) xs                         

renderLines start xs =
    foldM (\lineNum line -> mvAddStr (fst start) lineNum line) (snd start) $ lines xs


parseString :: String -> FormattedText
parseString [] = []
parseString ('<':xs) = let (tag, rest) = break (=='>') xs in
                       case parseTag True xs of
                         Nothing -> parseString (drop 1 rest)
                         Just a -> a : parseString (drop 1 rest)
parseString xs = let (text, rest) = break (=='<') xs in 
                 TextString text : parseString rest

parseTag fg [] = Nothing
parseTag fg xss@(x:xs)
    | isPrefixOf "/" xss = Just Pop
    | isPrefixOf "bg:" xss = parseTag False (drop 3 xss)
    | isPrefixOf "black" xss = Just $ field Black
    | isPrefixOf "red" xss = Just $ field Red
    | isPrefixOf "green" xss = Just $ field Green
    | isPrefixOf "yellow" xss = Just $ field Yellow
    | isPrefixOf "blue" xss = Just $ field Blue
    | isPrefixOf "purple" xss = Just $ field Purple
    | isPrefixOf "cyan" xss = Just $ field Cyan
    | isPrefixOf "grey" xss = Just $ field Grey
    | isPrefixOf "bright" xss = Just $ TextBold True
    | isPrefixOf "dim" xss = Just $ TextBold False
    | isPrefixOf "brightBg" xss = Just $ TextBoldBg True
    | isPrefixOf "dimBg" xss = Just $ TextBoldBg False
    | otherwise = Nothing
    where field | fg = TextFgColor
                | otherwise = TextBgColor

--parseString ('"':xs) = 
--    let (begin, end) = break (=='"') xs in
--    TextString ('"' : begin) : parseString end

wordWrap = wrapLine'

-- shamelessly stolen from I forget where :( (replaces some truly horrible, though technically more powerful, code)
wrapLine' :: Int -> String -> [String]
wrapLine' maxLen line = map unwords $ gobble 0 [] $ words line
    where
      gobble :: Int -> [String] -> [String] -> [[String]]
      gobble k acc [] = [reverse acc]
      gobble k acc ws@(w:rest) 
          | l >= maxLen     = addLastLine ([w] : gobbleRest rest)
          | k + l >= maxLen = addLastLine $ gobbleRest ws
          | otherwise       = gobble (k + l + 1) (w : acc) rest
          where l = length w
                addLastLine = case acc of
                                [] -> id
                                _ -> (reverse acc :)
                gobbleRest [] = []
                gobbleRest xs = gobble 0 [] xs
