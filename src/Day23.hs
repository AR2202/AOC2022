module Day23
  ( surroundingPositions
  , makeMove
  , largerExample
  , makeMoves
  , minx
  , maxy
  , empty10rounds
  , proposeMoveN
  , proposeMoveS
  , proposedMoves
  , input23
  , day23A
  , day23B
  ) where

import           Common
import           Data.List       (foldl', (\\))
import qualified Data.Map.Strict as M
import qualified Data.Set        as S

------Types -----
type ElfPosition = (Int, Int)

data Direction
  = N
  | S
  | W
  | E
  | NE
  | NW
  | SE
  | SW
  deriving (Show, Read, Eq)

data FreePos
  = AllFree
  | NorthFree
  | SouthFree
  | WestFree
  | EastFree
  | NoneFree
  deriving (Show, Read, Eq)

surroundingPositions :: ElfPosition -> [ElfPosition]
surroundingPositions (x, y) =
  [(x1, y1) | x1 <- [x - 1 .. x + 1], y1 <- [y - 1 .. y + 1]] \\ [(x, y)]

northernPositions :: ElfPosition -> [ElfPosition]
northernPositions (x, y) = [(x1, y + 1) | x1 <- [x - 1 .. x + 1]]

southernPositions :: ElfPosition -> [ElfPosition]
southernPositions (x, y) = [(x1, y - 1) | x1 <- [x - 1 .. x + 1]]

westernPositions :: ElfPosition -> [ElfPosition]
westernPositions (x, y) = [(x - 1, y1) | y1 <- [y - 1 .. y + 1]]

easternPositions :: ElfPosition -> [ElfPosition]
easternPositions (x, y) = [(x + 1, y1) | y1 <- [y - 1 .. y + 1]]

proposeMoveN elfs elfpos@(x, y)
  | all (`S.notMember` elfs) (surroundingPositions elfpos) = elfpos
  | all (`S.notMember` elfs) (northernPositions elfpos) = (x, y + 1)
  | all (`S.notMember` elfs) (southernPositions elfpos) = (x, y - 1)
  | all (`S.notMember` elfs) (westernPositions elfpos) = (x - 1, y)
  | all (`S.notMember` elfs) (easternPositions elfpos) = (x + 1, y)
  | otherwise = elfpos

proposeMoveS elfs elfpos@(x, y)
  | all (`S.notMember` elfs) (surroundingPositions elfpos) = elfpos
  | all (`S.notMember` elfs) (southernPositions elfpos) = (x, y - 1)
  | all (`S.notMember` elfs) (westernPositions elfpos) = (x - 1, y)
  | all (`S.notMember` elfs) (easternPositions elfpos) = (x + 1, y)
  | all (`S.notMember` elfs) (northernPositions elfpos) = (x, y + 1)
  | otherwise = elfpos

proposeMoveW elfs elfpos@(x, y)
  | all (`S.notMember` elfs) (surroundingPositions elfpos) = elfpos
  | all (`S.notMember` elfs) (westernPositions elfpos) = (x - 1, y)
  | all (`S.notMember` elfs) (easternPositions elfpos) = (x + 1, y)
  | all (`S.notMember` elfs) (northernPositions elfpos) = (x, y + 1)
  | all (`S.notMember` elfs) (southernPositions elfpos) = (x, y - 1)
  | otherwise = elfpos

proposeMoveE elfs elfpos@(x, y)
  | all (`S.notMember` elfs) (surroundingPositions elfpos) = elfpos
  | all (`S.notMember` elfs) (easternPositions elfpos) = (x + 1, y)
  | all (`S.notMember` elfs) (northernPositions elfpos) = (x, y + 1)
  | all (`S.notMember` elfs) (southernPositions elfpos) = (x, y - 1)
  | all (`S.notMember` elfs) (westernPositions elfpos) = (x - 1, y)
  | otherwise = elfpos

proposedMoves f elfs =
  foldl' (\m key -> M.insertWith (+) key 1 m) M.empty $
  map (f elfs) $ S.toList elfs

makeMove elfs f =
  S.map
    (\elf ->
       if M.findWithDefault 0 (f elfs elf) posmap > 1
         then elf
         else f elfs elf)
    elfs
  where
    posmap = proposedMoves f elfs

makeMoves n elfs =
  foldl' makeMove elfs $
  take n $ cycle [proposeMoveN, proposeMoveS, proposeMoveW, proposeMoveE]

minx elfs = S.findMin $ S.map fst elfs

miny elfs = S.findMin $ S.map snd elfs

maxx elfs = S.findMax $ S.map fst elfs

maxy elfs = S.findMax $ S.map snd elfs

emptycells elfs =
  (maxx elfs + 1 - minx elfs) * (maxy elfs + 1 - miny elfs) - (S.size elfs)

empty10rounds elfs = emptycells $ makeMoves 10 elfs

---parse Input-----
input23 =
  map snd .
  filter (\x -> fst x == '#') .
  map regroup .
  concat . zipWith (flip zip . repeat) [1 ..] . map (flip zip [1 ..]) . reverse <$>
  splitLines "Day23.txt"

day23A = empty10rounds . S.fromList <$> input23

regroup :: ((a, b), c) -> ((a, (b, c)))
regroup ((x, y), z) = (x, (y, z))

-----Part 2------
roundNoMoves ::
     S.Set ElfPosition
  -> [(S.Set ElfPosition -> (Int, Int) -> (Int, Int))]
  -> Int
  -> Int
roundNoMoves elfs (x:xs) nrounds
  | makeMove elfs x == elfs = nrounds
  | otherwise = roundNoMoves (makeMove elfs x) xs (nrounds + 1)

solve23B startset =
  roundNoMoves
    startset
    (cycle [proposeMoveN, proposeMoveS, proposeMoveW, proposeMoveE])
    1

day23B = solve23B . S.fromList <$> input23

largerExample :: S.Set ElfPosition
largerExample =
  S.fromList
    [ (2, 1)
    , (5, 1)
    , (1, 2)
    , (2, 2)
    , (4, 2)
    , (6, 2)
    , (7, 2)
    , (1, 3)
    , (3, 3)
    , (4, 3)
    , (5, 3)
    , (2, 4)
    , (6, 4)
    , (7, 4)
    , (1, 5)
    , (5, 5)
    , (7, 5)
    , (3, 6)
    , (4, 6)
    , (5, 6)
    , (7, 6)
    , (5, 7)
    ]
