{-# LANGUAGE FlexibleContexts #-}

module Day12
  ( coordChars2CoordElevations,
    solve12Part1,
    day12A,
  )
where

import Common
import Data.List (nub, (\\))
import qualified Data.Map as M

-- Types----
type Coord = (Int, Int)

type HillGraph = M.Map Coord [Coord]

type Dists = M.Map Coord Int

---Part 1----
day12A :: IO ()
day12A = solve12Part1 "Day12.txt"

example12A :: IO ()
example12A = solve12Part1 "Example12.txt"

solve12Part1 :: String -> IO ()
solve12Part1 filename = do
  withCoords <- loadAndAddCoords filename
  let elevationmap = coordChars2ElevationMap withCoords
  let hillgraph = constructHillGraph elevationmap
  let startc = startcoord withCoords
  let finishc = finishcoord withCoords
  let shortestPath = pathlength hillgraph startc finishc
  print shortestPath

startcoord coordlist = fst $ head $ filter (\x -> snd x == 'S') coordlist

finishcoord coordlist = fst $ head $ filter (\x -> snd x == 'E') coordlist

elevations :: M.Map Char Int
elevations = M.insert 'S' 1 $ M.insert 'E' 26 $ M.fromList $ zip ['a' ..] [1 ..]

char2elevation :: Char -> Int
char2elevation c = M.findWithDefault 0 c elevations

coordChar2CoordElevation :: (Coord, Char) -> (Coord, Int)
coordChar2CoordElevation (coord, c) = (coord, char2elevation c)

coordChars2CoordElevations :: [(Coord, Char)] -> [(Coord, Int)]
coordChars2CoordElevations = map coordChar2CoordElevation

coordChars2ElevationMap :: [(Coord, Char)] -> Dists
coordChars2ElevationMap = M.fromList . coordChars2CoordElevations

pathlength :: HillGraph -> Coord -> Coord -> Int
pathlength graph startcoord endcoord = pathlength' endcoord [startcoord] graph (M.singleton startcoord 0) 0

pathlength' :: Coord -> [Coord] -> HillGraph -> Dists -> Int -> Int
pathlength' endcoord [] g dists currentDist = M.findWithDefault (-1) endcoord dists
pathlength' endcoord toVisit g dists currentDist
  | M.member endcoord dists = M.findWithDefault (-1) endcoord dists
  | otherwise =
      pathlength'
        endcoord
        ( filter (`M.notMember` dists) $
            nub $
              concatMap (\x -> M.findWithDefault [] x g) toVisit
        )
        g
        (M.union dists (M.fromList (zip toVisit (repeat currentDist))))
        (currentDist + 1)

reachable :: (Coord, Int) -> Dists -> [Coord]
reachable (coord, i) elevationmap = filter isReachable $ neighbours coord
  where
    neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    isReachable x = M.findWithDefault 0 x elevationmap - i <= 1

reachables :: Dists -> (Coord, Int) -> (Coord, [Coord])
reachables elevationmap (coord, i) = (coord, reachable (coord, i) elevationmap)

constructHillGraph :: Dists -> HillGraph
constructHillGraph elevationmap = M.fromList $ map (reachables elevationmap) $ M.toList elevationmap
