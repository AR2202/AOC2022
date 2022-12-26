module Day25
  ( day25A
  ) where

import           Common

----Parsing input------
day25A =
  addNumbers' . map reverse . (map . map) readAsNumbers <$>
  splitLines "Day25.txt"

readAsNumbers :: Char -> Int
readAsNumbers '1' = 1
readAsNumbers '2' = 2
readAsNumbers '0' = 0
readAsNumbers '-' = -1
readAsNumbers '=' = -2

addNumbers' lists = addNumbers lists 0 []

addNumbers :: [[Int]] -> Int -> [String] -> [Char]
addNumbers [] 0 output = concat output
addNumbers [] carry output = addNumbers [] newcarry' (newdigit' : output)
  where
    (newcarry', newdigit') = convert carry
addNumbers numberlists carry output =
  addNumbers
    ((filter (not . null) . (map tail)) numberlists)
    newcarry
    (newdigit : output)
  where
    (newcarry, newdigit) = convert $ (+) carry $sum $ map head numberlists

convert :: Int -> (Int, String)
convert x =
  case x `mod` 5 of
    3 -> (x `div` 5 + 1, "=")
    4 -> (x `div` 5 + 1, "-")
    y -> (x `div` 5, show y)
