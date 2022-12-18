module Day17
  ( example17A
  , day17A
  ) where

import           Common
import           Data.Bifunctor (bimap)
import qualified Data.Map       as M
import qualified Data.Set       as S

---Types---
data Airstream
  = L
  | R
  deriving (Show, Read, Eq)

data Rock
  = First
  | Second
  | Third
  | Forth
  | Fifth
  deriving (Show, Read, Eq)

data RockTower =
  RockTower
    { occupied  :: S.Set (Int, Int)
    , maxheight :: Int
    }
  deriving (Show, Read, Eq)

type RockState = [(Int, Int)]

-----PArt 1-------
day17A :: IO ()
day17A = solve17A "Day17.txt"

example17A :: IO ()
example17A = solve17A "Example17.txt"

solve17A :: String -> IO ()
solve17A filename = do
  filecontents <- loadInput filename
  let airstream = cycle $toAirstream filecontents
  print $ nRocksFalling rocks2022 airstream floorRocks

blowRock :: RockState -> Airstream -> RockTower -> RockState
blowRock rockstate L rocktower =
  if (any (`S.member` (occupied rocktower)) $moveleft rockstate) ||
     (any (\x -> (fst x) < 0) $moveleft rockstate)
    then rockstate
    else moveleft rockstate
blowRock rockstate R rocktower =
  if (any (`S.member` (occupied rocktower)) $moveright rockstate) ||
     (any (\x -> (fst x) > 6) $moveright rockstate)
    then rockstate
    else moveright rockstate

moveleft :: RockState -> RockState
moveleft = map (bimap (\x -> x - 1) id)

moveright :: RockState -> RockState
moveright = map (bimap (+ 1) id)

movedown :: RockState -> RockState
movedown = map (bimap id (\x -> x - 1))

nRocksFalling :: [Rock] -> [Airstream] -> RockTower -> Int
nRocksFalling [] _ rockTower = maxheight rockTower
nRocksFalling (x:xs) airstream rockTower =
  nRocksFalling
    xs
    restairstream
    (RockTower
       (S.union rockrestpos (occupied rockTower))
       (maximum [maxheight rockTower, maxnewrock]))
  where
    (restairstream, rockrestpos, maxnewrock) =
      rockRestPos (initialRockState x rockTower) airstream rockTower

initialRockState r rockTower =
  map (bimap id (+ (4 + maxheight rockTower))) $ coordinates r

coordinates First  = [(2, 0), (3, 0), (4, 0), (5, 0)]
coordinates Second = [(3, 0), (2, 1), (3, 1), (4, 1), (3, 2)]
coordinates Third  = [(2, 0), (3, 0), (4, 0), (4, 1), (4, 2)]
coordinates Forth  = [(2, 0), (2, 1), (2, 2), (2, 3)]
coordinates Fifth  = [(2, 0), (2, 1), (3, 0), (3, 1)]

rockRestPos rockstate (x:xs) rockTower =
  if any (`S.member` (occupied rockTower)) $
     movedown $ blowRock rockstate x rockTower
    then ( xs
         , S.fromList (blowRock rockstate x rockTower)
         , (maximum . map snd) (blowRock rockstate x rockTower))
    else rockRestPos (movedown $ blowRock rockstate x rockTower) xs rockTower

rocks2022 = take 2022 $ cycle [First, Second, Third, Forth, Fifth]

floorRocks =
  RockTower
    (S.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)])
    0

----parse Input-----
toAirstream :: String -> [Airstream]
toAirstream ""       = []
toAirstream ('>':xs) = R : toAirstream xs
toAirstream ('<':xs) = L : toAirstream xs
toAirstream (x:xs)   = toAirstream xs
