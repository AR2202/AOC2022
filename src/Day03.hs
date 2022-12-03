module Day03
  ( day3A
  , day3B
  ) where

import           Common
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Data.Maybe      (fromJust)
import qualified Data.Set        as S

------- Dictionary of priorities ----
priorities :: M.Map Char Int
priorities = M.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]

------finding Common elements of one backpack
findCommon :: String -> [Int]
findCommon backpack =
  let front = S.fromList $ take firsthalf itempriorities
      back = S.fromList $ drop firsthalf itempriorities
      itempriorities = map (fromJust . (`M.lookup` priorities)) backpack
      firsthalf = length backpack `div` 2
   in S.toList (front `S.intersection` back)

solve3Part1 :: String -> IO Int
solve3Part1 filename = sum . concatMap findCommon <$> splitLines filename

-------part 2 -------
findGroup :: [String] -> [Int]
findGroup list =
  S.toList $foldr1 S.intersection $
  map (S.fromList . map (fromJust . (`M.lookup` priorities))) list

solve3Part2 :: String -> IO Int
solve3Part2 filename =
  sum . concatMap findGroup . chunksOf 3 <$> splitLines filename

---solution-----
day3A :: IO ()
day3A = solve3Part1 "Day03.txt" >>= print

day3B :: IO ()
day3B = solve3Part2 "Day03.txt" >>= print
