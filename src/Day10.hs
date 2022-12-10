module Day10
  ( day10A
  ) where

import           Common
import           Data.List (foldl', nub, scanl')

----Types-------
data Instructions
  = Addx Int
  | Noop
  deriving (Show, Read, Eq)

-----parse input-----
parseInstructions :: String -> IO [Instructions]
parseInstructions filename = map (read . capitalize) <$> splitLines filename

----Part1-------
instructionsToX :: [Instructions] -> [Int]
instructionsToX []          = []
instructionsToX (Noop:xs)   = 0 : instructionsToX xs
instructionsToX (Addx a:xs) = 0 : a : instructionsToX xs

signalIntervals = map ((+ 20) . (* 40)) [0 ..]

sumXs :: [Int] -> [Int]
sumXs xs = map (sum . (`take` xs)) $takeWhile (<= length xs) signalIntervals

signalStrengths :: [Int] -> [Int]
signalStrengths xs = zipWith (*) intervals (map (sum . (`take` xs)) intervals)
  where
    intervals = takeWhile (<= length xs) signalIntervals

solve10a :: String -> IO Int
solve10a filename =
  sum . signalStrengths . (1 :) . instructionsToX <$> parseInstructions filename

day10A :: IO Int
day10A = solve10a "Day10.txt"
