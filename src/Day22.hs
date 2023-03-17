module Day22 (viewLeft, viewRight, testsequence, example22) where

import Common
import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Sequence as Seq

viewLeft (a Seq.:<| as) = a

viewRight (as Seq.:|> a) = a

testsequence = Seq.fromList [1, 2, 3, 4]

-- Types
-----------------------------
data Facing = R | L | D | U deriving (Show, Read, Eq)

data Turning = Clockwise | Anticlockwise | Stay

type Coord = (Int, Int)

type Position = (Coord, Facing)

data Tile = Wall | FreeTile deriving (Show, Read, Eq)

type Board = M.Map Coord Tile

---------------------------
-- Part 1
--------------------------
turn :: Facing -> Turning -> Facing
turn R Clockwise = D
turn R Anticlockwise = U
turn L Clockwise = U
turn L Anticlockwise = D
turn U Clockwise = R
turn U Anticlockwise = L
turn D Clockwise = L
turn D Anticlockwise = R
turn x Stay = x

turnPosition :: Position -> Turning -> Position
turnPosition (coord, facing) turning = (coord, turn facing turning)

moveRight :: Board -> Int -> Coord -> Coord
moveRight _ 0 coord = coord
moveRight board n (x, y) =
  case M.lookup (x + 1, y) board of
    Nothing -> case snd leftmost of
      Wall -> (x, y)
      _ -> moveRight board (n - 1) (fst leftmost)
    Just Wall -> (x, y)
    Just FreeTile -> moveRight board (n - 1) (x + 1, y)
  where
    leftmost = M.findMin $ M.filterWithKey (\k _ -> snd k == y) board

moveLeft :: Board -> Int -> Coord -> Coord
moveLeft _ 0 coord = coord
moveLeft board n (x, y) =
  case M.lookup (x - 1, y) board of
    Nothing -> case snd rightmost of
      Wall -> (x, y)
      _ -> moveLeft board (n - 1) (fst rightmost)
    Just Wall -> (x, y)
    Just FreeTile -> moveLeft board (n - 1) (x - 1, y)
  where
    rightmost = M.findMax $ M.filterWithKey (\k _ -> snd k == y) board

moveUp :: Board -> Int -> Coord -> Coord
moveUp _ 0 coord = coord
moveUp board n (x, y) =
  case M.lookup (x, y - 1) board of
    Nothing -> case snd downmost of
      Wall -> (x, y)
      _ -> moveUp board (n - 1) (fst downmost)
    Just Wall -> (x, y)
    Just FreeTile -> moveUp board (n - 1) (x, y - 1)
  where
    downmost = M.findMax $ M.filterWithKey (\k _ -> fst k == x) board

moveDown :: Board -> Int -> Coord -> Coord
moveDown _ 0 coord = coord
moveDown board n (x, y) =
  case M.lookup (x, y + 1) board of
    Nothing -> case snd upmost of
      Wall -> (x, y)
      _ -> moveDown board (n - 1) (fst upmost)
    Just Wall -> (x, y)
    Just FreeTile -> moveDown board (n - 1) (x, y + 1)
  where
    upmost = M.findMin $ M.filterWithKey (\k _ -> fst k == x) board

makeOneMove :: Board -> Position -> (Int, Turning) -> Position
makeOneMove board (coord, facing) (n, t) = (movefun facing board n coord, turn facing t)

makeAllMoves :: Board -> Position -> [(Int, Turning)] -> Position
makeAllMoves board = foldl' (makeOneMove board)

movefun :: Facing -> (Board -> Int -> Coord -> Coord)
movefun R = moveRight
movefun L = moveLeft
movefun U = moveUp
movefun D = moveDown

----------------------
-- Parsing Input-----
-----------------------

example22 = (extractBoardWithCoords <$> splitOnBlankLine "Example22.txt") >>= print

extractBoard = head

extractSequence = last

extractBoardWithCoords = M.fromList . map char2TilePos . filter (\x -> snd x `elem` "#.") . addCoordinates . lines . extractBoard

char2Tile :: Char -> Tile
char2Tile '.' = FreeTile
char2Tile '#' = Wall

char2TilePos :: (Coord, Char) -> (Coord, Tile)
char2TilePos (coord, c) = (coord, char2Tile c)
