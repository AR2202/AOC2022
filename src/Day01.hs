module Day01
  ( example1A
  , day1A
  , day1B
  ) where

import           Common
import           Data.List
import           Data.Ord

example1A :: IO ()
example1A = solve1Part1 "Example01.txt"

day1A :: IO ()
day1A = solve1Part1 "Day01.txt"

day1B :: IO ()
day1B = solve1Part2 "Day01.txt"

solve1Part1 :: String -> IO ()
solve1Part1 filename =
  maximum . map sum <$> splitOnBlankSplitAndRead filename >>= print

solve1Part2 :: String -> IO ()
solve1Part2 filename =
  sum . take 3 . sortOn Data.Ord.Down . map sum <$>
  splitOnBlankSplitAndRead filename >>=
  print
