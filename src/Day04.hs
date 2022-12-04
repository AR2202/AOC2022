module Day04
  ( splitInput4
  , day4A
  , day4B
  ) where

import           Common
import           Data.List.Split

--parse Input-----
splitInput4 :: String -> IO [((Int, Int), (Int, Int))]
splitInput4 filename =
  (map list2tuple) . (map . map) (list2tuple . map read . splitOn "-") <$>
  splitCommas filename

-----Part 1 ------
isContained :: (Int, Int) -> (Int, Int) -> Bool
isContained (x, y) (a, b) = a >= x && b <= y

isContainedSymmetric :: ((Int, Int), (Int, Int)) -> Bool
isContainedSymmetric (a, b) = isContained a b || isContained b a

solve4 :: (((Int, Int), (Int, Int)) -> Bool) -> String -> IO ()
solve4 f filename = length . filter f <$> splitInput4 filename >>= print

example4A :: IO ()
example4A = solve4 isContainedSymmetric "Example04.txt"

day4A :: IO ()
day4A = solve4 isContainedSymmetric "Day04.txt"

------Part 2------
overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (x, y) (a, b) = a >= x && a <= y

overlapsSymmetric :: ((Int, Int), (Int, Int)) -> Bool
overlapsSymmetric (a, b) = overlaps a b || overlaps b a

day4B :: IO ()
day4B = solve4 overlapsSymmetric "Day04.txt"
