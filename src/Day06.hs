module Day06
  ( example6A
  , day6A
  , day6B
  ) where

import           Common
import           Data.List (nub)

char2StartOfPacket :: Int -> String -> Int
char2StartOfPacket n [] = 0
char2StartOfPacket n (x:xs)
  | (length . nub) (take n (x : xs)) == n = n
  | otherwise = 1 + char2StartOfPacket n xs

solve6 :: Int -> String -> IO Int
solve6 n filename = char2StartOfPacket n <$> loadInput filename

example6A :: IO Int
example6A = solve6 4 "Example06.txt"

day6A :: IO Int
day6A = solve6 4 "Day06.txt"

day6B :: IO Int
day6B = solve6 14 "Day06.txt"
