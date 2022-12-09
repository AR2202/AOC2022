module Day08
  ( readInput
  , readAddIndices
  , allVisibleLeft
  , readTranspose
  , allVisible
  , example8A
  , day8A
  , readAsVec
  , example8B
  , day08B
  ) where

import           Common
import           Data.Bifunctor  (bimap)
import           Data.Char       (digitToInt)
import           Data.List       (foldl', nub, tails, transpose)
import           Data.List.Split (chunksOf)
import qualified Data.Vector     as V

----read Input-----
readInput :: String -> IO [[Int]]
readInput filename = (map . map) digitToInt . lines <$> loadInput filename

readAddIndices filename = addIndices <$> readInput filename

readTranspose filename = transpose <$> readAddIndices filename

allVisibleLeft filename = map visibleLeft <$> readAddIndices filename

readAsVec filename = map toVec <$> readAddIndices filename

------Part1----
example8A :: IO Int
example8A = allVisible "Example08.txt"

day8A :: IO Int
day8A = allVisible "Day08.txt"

allVisible filename = length . visible <$> readAddIndices filename

--creating indices to keep track of the trees
addIndices :: [[Int]] -> [[(Int, Int)]]
addIndices lists = zipWith zip (chunksOf ((length . head) lists) [1 ..]) lists

-- visible from the left -----
visibleLeft list =
  foldl'
    (\acc x ->
       if snd x > (snd . head) acc
         then x : acc
         else acc)
    (take 1 list)
    list

-- visible from the right ------
visibleRight list =
  foldr
    (\x acc ->
       if snd x > (snd . head) acc
         then x : acc
         else acc)
    [last list]
    list

visibleLR list = nub $visibleLeft list ++ visibleRight list

visible lists =
  nub $concatMap visibleLR lists ++ concatMap visibleLR (transpose lists)

toVec list = V.generate (length list) (\i -> list !! i)

------part 2------
--very inefficient - need improvement
rightView :: [(Int, Int)] -> [(Int, Int)]
rightView list = map go (tails list)
  where
    go :: [(Int, Int)] -> (Int, Int)
    go [] = (0, 0)
    go [x] = (fst x, 0)
    go (x:y:zs)
      | snd x <= snd y = (fst x, 1)
      | otherwise = (fst x, 1 + snd (go (x : zs)))

leftView list = rightView (reverse list)

views lists =
  concatMap leftView lists ++
  concatMap rightView lists ++
  concatMap leftView (transpose lists) ++ concatMap rightView (transpose lists)

viewscore lists =
  map (\x -> (product . map snd) (filter (\y -> x == fst y) listviews)) $
  map fst listviews
  where
    listviews = views lists

maxViewScore filename = maximum . viewscore <$> readAddIndices filename

example8B :: IO Int
example8B = maxViewScore "Example08.txt"

day08B :: IO Int
day08B = maxViewScore "Day08.txt"
