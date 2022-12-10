module Day09
  ( step
  , Directions(..)
  , Configuration(..)
  , input9
  , manySteps
  , manyStepsDirections
  , solve9A
  , day9A
  ) where

import           Common
import           Data.List (foldl', nub, scanl')

----Types-------
data Directions
  = R
  | L
  | D
  | U
  deriving (Show, Read, Eq)

data Configuration
  = Covers
  | Right'
  | Left'
  | Down
  | Up
  | LeftUp
  | RightUp
  | LeftDown
  | RightDown
  deriving (Show, Read, Eq)

type Coordinate = (Int, Int)

--taking a step-----
step :: (Configuration, Coordinate) -> Directions -> (Configuration, Coordinate)
step (Covers, coord) R     = (Right', coord)
step (Covers, coord) L     = (Left', coord)
step (Covers, coord) D     = (Down, coord)
step (Covers, coord) U     = (Up, coord)
step (Right', (x, y)) R    = (Right', (x + 1, y))
step (Right', coord) L     = (Covers, coord)
step (Right', coord) D     = (RightDown, coord)
step (Right', coord) U     = (RightUp, coord)
step (Left', coord) R      = (Covers, coord)
step (Left', (x, y)) L     = (Left', (x - 1, y))
step (Left', coord) D      = (LeftDown, coord)
step (Left', coord) U      = (LeftUp, coord)
step (Down, coord) R       = (RightDown, coord)
step (Down, coord) L       = (LeftDown, coord)
step (Down, (x, y)) D      = (Down, (x, y - 1))
step (Down, coord) U       = (Covers, coord)
step (Up, coord) R         = (RightUp, coord)
step (Up, coord) L         = (LeftUp, coord)
step (Up, coord) D         = (Covers, coord)
step (Up, (x, y)) U        = (Up, (x, y + 1))
step (LeftUp, coord) R     = (Up, coord)
step (LeftUp, (x, y)) L    = (Left', (x - 1, y + 1))
step (LeftUp, coord) D     = (Left', coord)
step (LeftUp, (x, y)) U    = (Up, (x - 1, y + 1))
step (RightUp, (x, y)) R   = (Right', (x + 1, y + 1))
step (RightUp, coord) L    = (Up, coord)
step (RightUp, coord) D    = (Right', coord)
step (RightUp, (x, y)) U   = (Up, (x + 1, y + 1))
step (LeftDown, coord) R   = (Down, coord)
step (LeftDown, (x, y)) L  = (Left', (x - 1, y - 1))
step (LeftDown, (x, y)) D  = (Down, (x - 1, y - 1))
step (LeftDown, coord) U   = (Left', coord)
step (RightDown, (x, y)) R = (Right', (x + 1, y - 1))
step (RightDown, coord) L  = (Down, coord)
step (RightDown, (x, y)) D = (Down, (x + 1, y - 1))
step (RightDown, coord) U  = (Right', coord)

manySteps ::
     (Directions, Int)
  -> (Configuration, Coordinate)
  -> (Configuration, Coordinate)
manySteps (dir, n) startconf = iterate (`step` dir) startconf !! n

expandSteps :: (Directions, Int) -> [Directions]
expandSteps (d, i) = replicate i d

manyStepsDirections ::
     [(Directions, Int)]
  -> (Configuration, Coordinate)
  -> [(Configuration, Coordinate)]
manyStepsDirections directions startconf =
  scanl' step startconf $ concatMap expandSteps directions

extractCoords :: [(Configuration, Coordinate)] -> [Coordinate]
extractCoords = map snd

---parsing input-----
input9 :: String -> IO [(Directions, Int)]
input9 filename = map (readTuple . list2tuple) <$> splitLinesAndWords filename

---Part1-----
solve9A :: String -> IO Int
solve9A filename =
  fmap
    (length . nub . extractCoords . (flip manyStepsDirections (Covers, (0, 0))))
    (input9 filename)

day9A :: IO Int
day9A = solve9A "Day09.txt"
