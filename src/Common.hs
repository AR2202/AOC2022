{-# LANGUAGE OverloadedStrings #-}

module Common
  ( splitOnBlankLine,
    splitOnBlankSplitAndRead,
    dir,
    filepath,
    loadInput,
    splitLines,
    splitLinesAndWords,
    list2tuple,
    readTuple,
    splitCommas,
    capitalize,
    list2triple,
    makeCoordinates,
    addCoordinates,
    loadAndAddCoords,
  )
where

import Data.Char (toUpper)
import Data.List (transpose)
import Data.List.Split

dir :: String
dir = "input/"

filepath :: String -> String
filepath filename = dir ++ filename

loadInput :: String -> IO String
loadInput filename = readFile $ filepath filename

splitOnBlankLine :: String -> IO [String]
splitOnBlankLine filename = splitOn "\n\n" <$> loadInput filename

splitOnBlankSplitAndRead :: String -> IO [[Int]]
splitOnBlankSplitAndRead filename = do
  textblocks <- splitOnBlankLine filename
  let linesInBlocks = map lines textblocks
  let converted = (map . map) read linesInBlocks
  return converted

splitLines :: String -> IO [String]
splitLines filename = lines <$> loadInput filename

splitLinesAndWords :: String -> IO [[String]]
splitLinesAndWords filename = map words <$> splitLines filename

-- partial function - will fail on lists with less than 2 elements
list2tuple :: [a] -> (a, a)
list2tuple (x : y : zs) = (x, y)

list2triple :: [a] -> (a, a, a)
list2triple (x : y : z : zs) = (x, y, z)

readTuple :: (Read a, Read b) => (String, String) -> (a, b)
readTuple (x, y) = (read x, read y)

splitCommas :: String -> IO [[String]]
splitCommas filename = map (splitOn ",") . lines <$> loadInput filename

capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

makeCoordinates :: Int -> Int -> [(Int, Int)]
makeCoordinates x y = (,) <$> [1 .. x] <*> [1 .. y]

addCoordinates :: [String] -> [((Int, Int), Char)]
addCoordinates ls = zip coordlist chars
  where
    coordlist = makeCoordinates ((length . head) ls) (length ls)
    chars = (concat . transpose) ls

loadAndAddCoords :: String -> IO [((Int, Int), Char)]
loadAndAddCoords filename = addCoordinates <$> splitLines filename
