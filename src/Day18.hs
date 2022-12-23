module Day18
  ( solve18A
  , day18A
  , User(..)
  , createUser
  , createList
  ) where

import           Common
import           Data.List       (sort)
import           Data.List.Split (splitOn)

data User =
  User
    { name   :: String
    , friend :: User
    }
  deriving (Show)

createUser =
  let user1 = User "annika" user2
      user2 = User "hugo" user1
   in user1

createList =
  let a = 1 : b
      b = 0 : a
   in a

----PArt 1------
day18A :: IO ()
day18A = solve18A "Day18.txt" >>= print

solve18A :: String -> IO Int
solve18A filename = nonAdjacent <$> toTripleList filename

countAdjacent :: [(Int, Int, Int)] -> Int
countAdjacent [] = 0
countAdjacent [x] = 0
countAdjacent ((x1, y1, z1):(x2, y2, z2):xs)
  | x1 == x2 && y1 == y2 && z2 - z1 == 1 = 2 + countAdjacent ((x2, y2, z2) : xs)
  | otherwise = countAdjacent ((x2, y2, z2) : xs)

swapTriples (x, y, z) = (y, z, x)

totalAdjacent :: [(Int, Int, Int)] -> Int
totalAdjacent list =
  (countAdjacent . sort) list + (countAdjacent . sort . map swapTriples) list +
  (countAdjacent . sort . map swapTriples . map swapTriples) list

nonAdjacent :: [(Int, Int, Int)] -> Int
nonAdjacent list = 6 * length list - totalAdjacent list

example :: [(Int, Int, Int)]
example = [(1, 1, 1), (2, 1, 1)]

----parsing input-------
toTripleList :: String -> IO [(Int, Int, Int)]
toTripleList filename =
  map (list2triple . map read . splitOn ",") <$> splitLines filename
