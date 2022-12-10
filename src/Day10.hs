module Day10
  ( day10A
  , solve10b
  ) where

import           Common
import           Data.List       (foldl', nub, scanl1)
import           Data.List.Split (chunksOf)

----Types-------
data Instructions
  = Addx Int
  | Noop
  deriving (Show, Read, Eq)

data PixelState
  = Lit
  | Dark
  deriving (Read, Eq)

instance Show PixelState where
  show Lit  = "@"
  show Dark = "."

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

--PArt2-----
currentX :: [Int] -> [Int]
currentX = scanl1 (+)

inSprite :: Int -> Int -> Bool
inSprite clockcycle pos = (horizontal >= pos - 1) && (horizontal <= pos + 1)
  where
    horizontal = clockcycle `mod` 40

lit :: [Int] -> [Bool]
lit = zipWith inSprite [0 ..]

bool2pixelstate :: Bool -> PixelState
bool2pixelstate True  = Lit
bool2pixelstate False = Dark

solve10b :: String -> IO ()
solve10b filename =
  chunksOf 40 .
  concatMap (show . bool2pixelstate) . lit . currentX . (1 :) . instructionsToX <$>
  parseInstructions filename >>=
  mapM_ putStrLn
