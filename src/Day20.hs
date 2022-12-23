module Day20
  ( mixNumbersList
  , example
  , makeStartAndMove1
  , makeStartMap
  , moveNumber
  , addPositions
  , nthnumber
  , sumOfnumbers
  , readInputSolve
  , solve20B
  , day20A
  , day20B
  ) where

import           Common
import qualified Data.IntMap as IM
import           Data.List   (foldl')

------Types-------
data PositionMaps =
  PositionMaps
    { currentPositions  :: IM.IntMap Int
    , originalPositions :: IM.IntMap Int
    }
  deriving (Show, Read, Eq)

makeStartMap listlen =
  PositionMaps (startPositions listlen) (startPositions listlen)

example :: [Integer]
example = [1, 2, -3, 3, -2, 0, 4]

addPositions :: [Integer] -> [(Int, Integer)]
addPositions list = zip [0 ..] list

startPositions :: Int -> IM.IntMap Int
startPositions listLen = IM.fromDistinctAscList $ zip [0 .. listLen - 1] [0 ..]

moveNumber :: (Int, Integer) -> PositionMaps -> PositionMaps
moveNumber (originalpos, number) positions =
  if newpos' >= currentpos
    then movenumbers (newpos' - currentpos) currentpos positions
    else movenumbersback (newpos' - currentpos) currentpos positions
  where
    currentpos = currpos IM.! originalpos
    currpos = currentPositions positions
    newpos =
      fromIntegral $
      (fromIntegral (currentpos) + number) `mod`
      (fromIntegral (IM.size currpos - 1)) +
      (fromIntegral (IM.size currpos - 1)) `mod`
      (fromIntegral (IM.size currpos - 1))
    newpos' =
      if newpos == 0
        then 6
        else newpos

movenumbers 0 currentposition positions = positions
movenumbers n currentposition positions =
  movenumbers (n - 1) (currentposition + 1) (PositionMaps newcurr neworig)
  where
    newcurr =
      IM.insert valueAtCurrent newpos $
      IM.insert valueaftercurrent currentpos curr
    neworig =
      IM.insert newpos valueAtCurrent $
      IM.insert currentpos valueaftercurrent orig
    valueAtCurrent = orig IM.! currentpos
    valueaftercurrent = orig IM.! newpos
    currentpos = currentposition
    newpos = (currentposition + 1)
    curr = currentPositions positions
    orig = originalPositions positions

movenumbersback 0 currentposition positions = positions
movenumbersback n currentposition positions =
  movenumbersback (n + 1) (currentposition - 1) (PositionMaps newcurr neworig)
  where
    newcurr =
      IM.insert valueAtCurrent newpos $
      IM.insert valueaftercurrent currentpos curr
    neworig =
      IM.insert newpos valueAtCurrent $
      IM.insert currentpos valueaftercurrent orig
    valueAtCurrent = orig IM.! currentpos
    valueaftercurrent = orig IM.! newpos
    currentpos = currentposition
    newpos = currentposition - 1
    curr = currentPositions positions
    orig = originalPositions positions

mixNumbersList :: [Integer] -> PositionMaps
mixNumbersList list =
  foldl' (flip moveNumber) (makeStartMap (length list)) (addPositions list)

mixNumbersList10x :: [Integer] -> PositionMaps
mixNumbersList10x list =
  foldl'
    (flip moveNumber)
    (makeStartMap (length list))
    (concat $ replicate 10 $ addPositions list)

makeStartAndMove1 list =
  moveNumber ((head . addPositions) list) (makeStartMap (length list))

zeroPosition :: [(Int, Integer)] -> Int
zeroPosition = fst . head . filter (\x -> snd x == 0)

currentPositionFromOriginalPosition pos positions =
  (currentPositions positions) IM.! pos

originalPositionFromCurrentPosition pos positions =
  (originalPositions positions) IM.! pos

nthnumber n list positions =
  snd $
  list !!
  (originalPositionFromCurrentPosition
     ((currentPositionFromOriginalPosition (zeroPosition list) positions + n) `mod`
      (fromIntegral (length list)))
     positions)

sumOfnumbers list positions =
  nthnumber 1000 list positions + nthnumber 2000 list positions +
  nthnumber 3000 list positions

solve20A l = sumOfnumbers (addPositions l) (mixNumbersList l)

solve20B l =
  sumOfnumbers (addPositions lmultiplied) (mixNumbersList10x lmultiplied)
  where
    lmultiplied = map (* decryptionKey) l

readInputSolve f filename = f . map read <$> splitLines filename

day20A = readInputSolve solve20A "Day20.txt"

day20B = readInputSolve solve20B "Day20.txt"

decryptionKey = 811589153
