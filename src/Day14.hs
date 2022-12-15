module Day14
  ( numSand
  , nums
  , solve14A
  , solve14B
  ) where

import           Common
import           Data.Either                              (fromRight)
import           Data.List.Split                          (splitOn)
import qualified Data.Set                                 as S
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator

--types-
type Point = (Int, Int)

type Occupied = S.Set Point

data RestState
  = Rest Point
  | Norest
  deriving (Read, Show, Eq)

---PArt1---
solve14A filename =
  numSand 0 . S.fromList . concatMap nums <$> splitLines filename

reststate :: Occupied -> RestState
reststate occupied = go (500, 0) occupied
  where
    go (x, y) occupied
      | y > (S.findMax (S.map snd occupied)) = Norest
      | S.member (x, y + 1) occupied &&
          S.member (x - 1, y + 1) occupied && S.member (x + 1, y + 1) occupied =
        Rest (x, y)
      | S.member (x, y + 1) occupied && S.member (x - 1, y + 1) occupied =
        go (x + 1, y + 1) occupied
      | S.member (x, y + 1) occupied = go (x - 1, y + 1) occupied
      | otherwise = go (x, y + 1) occupied

numSand :: Int -> Occupied -> Int
numSand n occupied =
  case reststate occupied of
    Norest -> n
    Rest p -> numSand (n + 1) (S.insert p occupied)

---Part2------
solve14B filename =
  numSandB 0 . S.fromList . concatMap nums <$> splitLines filename

reststateB :: Occupied -> Int -> RestState
reststateB occupied ymax = go (500, 0) occupied ymax
  where
    go (x, y) occupied ymax
      | y > ymax = Rest (x, y)
      | S.member (x, y + 1) occupied &&
          S.member (x - 1, y + 1) occupied && S.member (x + 1, y + 1) occupied =
        Rest (x, y)
      | S.member (x, y + 1) occupied && S.member (x - 1, y + 1) occupied =
        go (x + 1, y + 1) occupied ymax
      | S.member (x, y + 1) occupied = go (x - 1, y + 1) occupied ymax
      | otherwise = go (x, y + 1) occupied ymax

numSandB :: Int -> Occupied -> Int
numSandB n occupied = go n (S.findMax (S.map snd occupied)) occupied
  where
    go n ymax occupied =
      case reststateB occupied ymax of
        Rest (500, 0) -> n + 1
        Rest p        -> go (n + 1) ymax (S.insert p occupied)

----Parser------
nums :: String -> [Point]
nums line =
  addMissing $ map (readTuple . list2tuple . (splitOn ",")) $splitOn " -> " line

addMissing :: [Point] -> [Point]
addMissing [] = []
addMissing [x] = [x]
addMissing ((x1, y1):(x2, y2):xs)
  | x1 == x2 = addys x1 y1 y2 ++ addMissing ((x1, y2) : xs)
  | y1 == y2 = addxs x1 x2 y1 ++ addMissing ((x2, y1) : xs)

addys x y1 y2 = zip (repeat x) [minimum [y1, y2] .. maximum [y1, y2]]

addxs x1 x2 y = zip [minimum [x1, x2] .. maximum [x1, x2]] (repeat y)
