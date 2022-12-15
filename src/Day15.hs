module Day15
  ( covered
  , coveredInRow
  , parseAsSensor
  , Sensor(..)
  , input15
  , example15A
  , day15A
  ) where

import           Common
import qualified Data.Set                                 as S
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

import           Data.Either                              (fromRight)
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator

----Types------
type Point = (Int, Int)

data Sensor =
  Sensor
    { location :: Point
    , closest  :: Point
    }
  deriving (Show, Read, Eq)

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

covered :: Point -> Int -> S.Set Point
covered (px, py) dist =
  S.fromList
    [ (x, y)
    | x <- [px - dist .. px + dist]
    , y <- [py - dist .. py + dist]
    , manhattanDistance (px, py) (x, y) <= dist
    ]

covers :: Sensor -> S.Set Point
covers s =
  covered (location s) (manhattanDistance (location s) (closest s)) S.\\
  S.singleton (closest s)

coveredInRow :: Point -> Int -> Int -> S.Set Point
coveredInRow (px, py) dist row =
  S.fromList
    [ (x, row)
    | x <- [px - dist .. px + dist]
    , manhattanDistance (px, py) (x, row) <= dist
    ]

coversInRow :: Int -> Sensor -> S.Set Point
coversInRow row s =
  coveredInRow (location s) (manhattanDistance (location s) (closest s)) row S.\\
  S.singleton (closest s)

allCoverInRow :: Int -> [Sensor] -> S.Set Point
allCoverInRow row sensorlist = S.unions $map (coversInRow row) sensorlist

----Parsing-----
xpositionParser :: Parser Int
xpositionParser =
  read <$>
  between (string "Sensor at x=") (char ',') (many1 (oneOf "0123456789-"))

ypositionParser :: Parser Int
ypositionParser =
  read <$> between (string "y=") (char ':') (many1 (oneOf "0123456789-"))

positionParser :: Parser Point
positionParser = do
  x <- xpositionParser
  space
  y <- ypositionParser
  space
  return (x, y)

xbeaconParser :: Parser Int
xbeaconParser =
  read <$>
  between
    (string "closest beacon is at x=")
    (char ',')
    (many1 (oneOf "0123456789-"))

ybeaconParser :: Parser Int
ybeaconParser =
  read <$>
  ((string "y=") *> (many1 (oneOf "0123456789-")) <* notFollowedBy (char ':'))

beaconParser :: Parser Point
beaconParser = do
  x <- xbeaconParser
  space
  y <- ybeaconParser
  spaces
  return (x, y)

sensorParser :: Parser Sensor
sensorParser = do
  pos <- positionParser
  beacon <- beaconParser
  return $ Sensor pos beacon

parseAsSensor :: String -> Either ParseError Sensor
parseAsSensor = parse sensorParser "test"

----read input-----
input15 :: String -> IO (Either ParseError [Sensor])
input15 filename = traverse parseAsSensor <$> splitLines filename

solve15A :: Int -> String -> IO (Either ParseError Int)
solve15A n filename =
  (fmap . fmap) (S.size . allCoverInRow n) $ input15 filename

example15A :: IO (Either ParseError Int)
example15A = solve15A 10 "Example15.txt"

day15A :: IO (Either ParseError Int)
day15A = solve15A 2000000 "Day15.txt"
