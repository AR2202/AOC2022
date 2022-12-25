module Day24
  ( example24
  , makeAllMoves
  , makeOneMove
  , input24
  , walls24
  , blizzards24
  , solve24A
  ) where

import           Common
import qualified Data.Set as S

-----Types--------
type Coordinate = (Int, Int)

data DirectionMoving
  = Down
  | Up
  | R
  | L
  deriving (Show, Read, Eq)

type Blizzard = (Coordinate, DirectionMoving)

type Blizzards = [Blizzard]

type Cost = Int

type Path = (Coordinate, Cost)

type Paths = S.Set Path

type Walls = S.Set Coordinate

data ValleyState =
  ValleyState
    { blizzardstate :: Blizzards
    , allPaths      :: Paths
    , walls         :: Walls
    , finishCoord   :: Coordinate
    , shortestPath  :: Maybe Int
    }
  deriving (Show, Read, Eq)

nextBlizzardPosition :: Blizzard -> Walls -> Blizzard
nextBlizzardPosition (blizzardcoord, dir) walls =
  if nextcoord blizzardcoord dir `S.member` walls
    then (oppositcoord blizzardcoord dir, dir)
    else (nextcoord blizzardcoord dir, dir)
  where
    nextcoord (x, y) Down = (x, y - 1)
    nextcoord (x, y) Up   = (x, y + 1)
    nextcoord (x, y) L    = (x - 1, y)
    nextcoord (x, y) R    = (x + 1, y)
    oppositcoord (x, y) Down = (x, maxy walls - 2)
    oppositcoord (x, y) Up   = (x, miny walls + 1)
    oppositcoord (x, y) R    = (minx walls + 1, y)
    oppositcoord (x, y) L    = (maxx walls - 1, y)

nextBlizzardPositions walls blizzards =
  map (`nextBlizzardPosition` walls) blizzards

minx walls = S.findMin $ S.map fst walls

miny walls = S.findMin $ S.map snd walls

maxx walls = S.findMax $ S.map fst walls

maxy walls = S.findMax $ S.map snd walls

neighbourcoords (x, y) =
  [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1), (x, y)]

blizzardcoordinate :: Blizzard -> Coordinate
blizzardcoordinate = fst

makeOneMove :: ValleyState -> ValleyState
makeOneMove valleystate =
  valleystate
    { allPaths =
        S.filter
          (\(coord, cost) -> coord /= finishCoord valleystate)
          allnewpaths
    , blizzardstate = newblizzardstate
    , shortestPath =
        minimumShortest (shortestPath valleystate) (shortestFinished)
    }
  where
    finishingpaths =
      S.filter (\(coord, cost) -> coord == finishCoord valleystate) allnewpaths
    shortestFinished = S.lookupMin $ S.map snd finishingpaths
    allnewpaths = S.unions $ S.map makeCoordMove $ allPaths valleystate
    newblizzardstate =
      nextBlizzardPositions (walls valleystate) (blizzardstate valleystate)
    makeCoordMove (coord, cost) =
      case fmap (<= cost) (shortestPath valleystate) of
        Just True -> S.empty
        _ ->
          S.fromList
            [ (newcoord, cost + 1)
            | newcoord <- neighbourcoords coord
            , newcoord `S.notMember` (walls valleystate) &&
                newcoord `notElem` (map blizzardcoordinate newblizzardstate)
            ]

minimumShortest Nothing Nothing   = Nothing
minimumShortest Nothing (Just x)  = Just x
minimumShortest (Just x) Nothing  = Just x
minimumShortest (Just x) (Just y) = Just $ minimum [x, y]

makeAllMoves :: ValleyState -> Maybe Int
makeAllMoves valleystate
  | null (allPaths valleystate) = shortestPath valleystate
  | shortestPath valleystate /= Nothing = shortestPath valleystate
  | otherwise = makeAllMoves (makeOneMove valleystate)

-----PArsing-----
solve24A = do
  startblizzards <- blizzards24
  startwalls <- walls24
  let startpaths = S.fromList [entryPoint]
  let startstate =
        ValleyState startblizzards startpaths startwalls finishPoint Nothing
  print $ makeAllMoves startstate

input24 =
  map swap .
  filter (\x -> fst x /= '.') .
  map regroup .
  concat . zipWith (flip zip . repeat) [1 ..] . map (flip zip [1 ..]) . reverse <$>
  splitLines "Day24.txt"

regroup :: ((a, b), c) -> ((a, (b, c)))
regroup ((x, y), z) = (x, (y, z))

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

toBlizzard (coord, 'v') = (coord, Down)
toBlizzard (coord, '^') = (coord, Up)
toBlizzard (coord, '<') = (coord, L)
toBlizzard (coord, '>') = (coord, R)

walls24 =
  S.fromList . (:) (2, 38) . map fst . filter (\x -> snd x == '#') <$> input24

blizzards24 = map toBlizzard . filter (\x -> snd x /= '#') <$> input24

entryPoint = ((2, 37), 0)

finishPoint = (101, 1)

------the complex example ----
example24 =
  ValleyState
    blizzardexample
    startpathexample
    wallexample
    finishexample
    shortestStart

blizzardexample =
  [ ((1, 1), L)
  , ((1, 2), R)
  , ((1, 4), R)
  , ((2, 1), Up)
  , ((2, 2), Down)
  , ((2, 3), L)
  , ((2, 4), R)
  , ((3, 1), Down)
  , ((4, 1), Up)
  , ((4, 2), R)
  , ((4, 4), L)
  , ((5, 1), Up)
  , ((5, 2), L)
  , ((5, 3), L)
  , ((5, 4), Up)
  , ((6, 1), R)
  , ((6, 2), R)
  , ((6, 3), L)
  , ((6, 4), L)
  ]

startpathexample = S.fromList [((1, 5), 0)]

finishexample = (6, 0)

shortestStart = Nothing

wallexample =
  S.fromList
    [ (0, 0)
    , (0, 1)
    , (0, 2)
    , (0, 3)
    , (0, 4)
    , (0, 5)
    , (0, 7)
    , (1, 0)
    , (1, 6)
    , (2, 0)
    , (3, 0)
    , (1, 0)
    , (2, 5)
    , (3, 0)
    , (4, 0)
    , (5, 0)
    , (7, 0)
    , (7, 1)
    , (7, 2)
    , (7, 3)
    , (7, 4)
    , (3, 5)
    , (4, 5)
    , (5, 5)
    , (6, 5)
    , (7, 5)
    ]
