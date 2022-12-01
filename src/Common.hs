{-# LANGUAGE OverloadedStrings #-}

module Common
  ( splitOnBlankLine
  , splitOnBlankSplitAndRead
  , dir
  , filepath
  , loadInput
  ) where

import           Data.List.Split

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
