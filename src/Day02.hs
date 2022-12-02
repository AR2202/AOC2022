module Day02
  ( example2A
  , day2A
  , day2B
  ) where

import           Common

------Types ---------
data Oponent
  = A
  | B
  | C
  deriving (Show, Read, Eq)

data Selection
  = X
  | Y
  | Z
  deriving (Show, Read, Eq)

data Winning
  = Lose
  | Draw
  | Win
  deriving (Show, Read, Eq)

type Score = Int

shapeScore :: Selection -> Score
shapeScore X = 1
shapeScore Y = 2
shapeScore Z = 3

outcomeScore :: Winning -> Score
outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win  = 6

winning :: Oponent -> Selection -> Winning
winning A X = Draw
winning A Y = Win
winning A Z = Lose
winning B X = Lose
winning B Y = Draw
winning B Z = Win
winning C X = Win
winning C Y = Lose
winning C Z = Draw

totalScore :: (Oponent, Selection) -> Score
totalScore (o, s) =
  let winscore = outcomeScore (winning o s)
      choicescore = shapeScore s
   in winscore + choicescore

-- part 2 -------
winning2Selection :: Oponent -> Selection -> Selection
winning2Selection A X = Z
winning2Selection A Y = X
winning2Selection A Z = Y
winning2Selection B x = x
winning2Selection C X = Y
winning2Selection C Y = Z
winning2Selection C Z = X

totalScore2 :: (Oponent, Selection) -> Score
totalScore2 (o, s) =
  let winscore = outcomeScore (winning o newS)
      newS = winning2Selection o s
      choicescore = shapeScore newS
   in winscore + choicescore

-------- input parsing -------
solve2 :: ((Oponent, Selection) -> Score) -> String -> IO Int
solve2 f filename =
  sum . map (f . readTuple . list2tuple) <$> splitLinesAndWords filename

solve2Part1 :: String -> IO Int
solve2Part1 = solve2 totalScore

solve2Part2 :: String -> IO Int
solve2Part2 = solve2 totalScore2

-------- solutions -------
example2A :: IO ()
example2A = solve2Part1 "Example02.txt" >>= print

day2A :: IO ()
day2A = solve2Part1 "Day02.txt" >>= print

day2B :: IO ()
day2B = solve2Part2 "Day02.txt" >>= print
