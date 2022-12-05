module Day05
  ( input5
  , day5A
  , day5B
  ) where

import           Common
import qualified Data.IntMap     as IM
import           Data.List       (foldl', transpose)
import           Data.List.Split (chunksOf)

-----parsing input-----
input5 f filename =
  (allInstructions f . processBoth . list2tuple <$> splitOnBlankLine filename) >>=
  print

processGraphic =
  stackDict . map constructStack . transpose . map (chunksOf 4) . lines

processInstructions = map instructionNumbers . map words . lines

processBoth (a, b) = (processGraphic a, processInstructions b)

constructStack [] = []
constructStack (x:xs)
  | '[' `elem` x =
    ((take 1 . drop 1 . dropWhile (/= '[')) x) : constructStack xs
  | otherwise = constructStack xs

--partial function, assuming valid input
instructionNumbers :: [String] -> (Int, Int, Int)
instructionNumbers wordlist =
  ( (read . head . drop 1) wordlist
  , (read . head . drop 3) wordlist
  , (read . head . drop 5) wordlist)

---construct dictionary of Stacks
stackDict list = IM.fromList $ zip [1 ..] list

-----Part 1----
moveParcels :: Int -> [String] -> [String] -> ([String], [String])
moveParcels n [] to     = ([], to)
moveParcels 0 from to   = (from, to)
moveParcels n (x:xs) to = moveParcels (n - 1) xs (x : to)

moveParcelsDict ::
     (Int -> [String] -> [String] -> ([String], [String]))
  -> IM.IntMap [String]
  -> (Int, Int, Int)
  -> IM.IntMap [String]
moveParcelsDict f intmap (n, keyfrom, keyto) =
  IM.insert keyfrom newfrom $ IM.insert keyto newto intmap
  where
    (newfrom, newto) =
      f
        n
        (IM.findWithDefault [] keyfrom intmap)
        (IM.findWithDefault [] keyto intmap)

allInstructions ::
     (Int -> [String] -> [String] -> ([String], [String]))
  -> (IM.IntMap [String], [(Int, Int, Int)])
  -> IM.IntMap [String]
allInstructions f (m, inst) = foldl' (moveParcelsDict f) m inst

day5A :: IO ()
day5A = input5 moveParcels "Day05.txt"

----Part 2------
moveParcelsInOrder :: Int -> [String] -> [String] -> ([String], [String])
moveParcelsInOrder n [] to   = ([], to)
moveParcelsInOrder n from to = (drop n from, (take n from) ++ to)

day5B :: IO ()
day5B = input5 moveParcelsInOrder "Day05.txt"
