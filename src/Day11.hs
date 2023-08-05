module Day11
  ( example11,
    turn,
    monkeyround,
    monkeyroundWithCount,
    example11Count,
    nRoundsWithCount,
    solve11A,
    solve11B,
    day11,
  )
where

import Common
import Data.List (foldl', foldl1, sort)
import Data.Sequence (Seq (..), (<|), (><), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

-- Types----
data Monkey = Monkey
  { monkeynumber :: Int,
    items :: Seq.Seq Int,
    operations :: Int -> Int,
    test :: Int -> Bool,
    nextTrue :: Int,
    nextFalse :: Int
  }

instance Show Monkey where
  show monkey =
    "Monkeynumber: "
      ++ show (monkeynumber monkey)
      ++ " items: "
      ++ show (items monkey)
      ++ " Next Monkey if Test true: "
      ++ show (nextTrue monkey)
      ++ " Next Monkey if Test false: "
      ++ show (nextFalse monkey)

type MonkeyChain = V.Vector Monkey

---Part 1----
inspectItem :: (Int -> Int) -> Int -> Int
inspectItem f = f

adjustWorry :: Int -> Int
adjustWorry i = i `div` 3

testItem :: (Int -> Bool) -> Int -> Int -> Int -> Int
testItem testfun iftrue iffalse worrylevel =
  if testfun worrylevel
    then iftrue
    else iffalse

divisibleBy n x = x `mod` n == 0

throwItem item from to monkeychain =
  monkeychain
    V.// [ (from, monkeyfrom {items = Seq.drop 1 (items monkeyfrom)}),
           (to, monkeyto {items = items monkeyto |> item})
         ]
  where
    monkeyfrom = monkeychain V.! from
    monkeyto = monkeychain V.! to

processItem :: (Int -> Int) -> Int -> MonkeyChain -> MonkeyChain
processItem adjustWorryfun monkeynumber monkeychain =
  throwItem
    adjusteditem
    monkeynumber
    (testItem testf t f adjusteditem)
    monkeychain
  where
    testf = test current
    current = monkeychain V.! monkeynumber
    operation = operations current
    t = nextTrue current
    f = nextFalse current
    adjusteditem =
      (adjustWorryfun . inspectItem operation) $ Seq.index (items current) 0

turn :: (Int -> Int) -> Int -> MonkeyChain -> MonkeyChain
turn adjustWorryfun monkeynumber monkeychain
  | Seq.null (items (monkeychain V.! monkeynumber)) = monkeychain
  | otherwise = turn adjustWorryfun monkeynumber (processItem adjustWorryfun monkeynumber monkeychain)

turnWithCount ::
  (Int -> Int) -> Int -> (MonkeyChain, V.Vector Int) -> (MonkeyChain, V.Vector Int)
turnWithCount adjustWorryfun monkeynumber (monkeychain, numturns)
  | Seq.null (items (monkeychain V.! monkeynumber)) = (monkeychain, numturns)
  | otherwise =
      turnWithCount
        adjustWorryfun
        monkeynumber
        ( processItem adjustWorryfun monkeynumber monkeychain,
          V.accum (+) numturns [(monkeynumber, 1)]
        )

monkeyround :: (Int -> Int) -> MonkeyChain -> MonkeyChain
monkeyround adjustWorryfun monkeychain =
  foldl' (flip (turn adjustWorryfun)) monkeychain [0 .. V.length monkeychain - 1]

monkeyroundWithCount ::
  (Int -> Int) -> (MonkeyChain, V.Vector Int) -> (MonkeyChain, V.Vector Int)
monkeyroundWithCount adjustWorryfun (monkeychain, counts) =
  foldl'
    (flip (turnWithCount adjustWorryfun))
    (monkeychain, counts)
    [0 .. V.length monkeychain - 1]

nRoundsWithCount :: (Int -> Int) -> Int -> MonkeyChain -> V.Vector Int
nRoundsWithCount adjustWorryfun n monkeychain =
  snd $
    iterate
      (monkeyroundWithCount adjustWorryfun)
      (monkeychain, V.fromList $ replicate (V.length monkeychain) 0)
      !! n

-- this is a bit of a hack - I was too lazy to implement the parser
monkey0 = Monkey 0 (Seq.fromList [79, 98]) (* 19) (divisibleBy 23) 2 3

monkey1 = Monkey 1 (Seq.fromList [54, 65, 75, 74]) (+ 6) (divisibleBy 19) 2 0

monkey2 = Monkey 2 (Seq.fromList [79, 60, 97]) (^ 2) (divisibleBy 13) 1 3

monkey3 = Monkey 3 (Seq.fromList [74]) (+ 3) (divisibleBy 17) 0 1

example11 = V.fromList [monkey0, monkey1, monkey2, monkey3]

example11Count :: (MonkeyChain, V.Vector Int)
example11Count = (example11, V.fromList [0, 0, 0, 0])

solve11A monkeylist =
  (product . take 2 . reverse . sort . V.toList) $
    nRoundsWithCount adjustWorry 20 monkeylist

m0 =
  Monkey 0 (Seq.fromList [52, 60, 85, 69, 75, 75]) (* 17) (divisibleBy 13) 6 7

m1 =
  Monkey 1 (Seq.fromList [96, 82, 61, 99, 82, 84, 85]) (+ 8) (divisibleBy 7) 0 7

m2 = Monkey 2 (Seq.fromList [95, 79]) (+ 6) (divisibleBy 19) 5 3

m3 = Monkey 3 (Seq.fromList [88, 50, 82, 65, 77]) (* 19) (divisibleBy 2) 4 1

m4 =
  Monkey
    4
    (Seq.fromList [66, 90, 59, 90, 87, 63, 53, 88])
    (+ 7)
    (divisibleBy 5)
    1
    0

m5 = Monkey 5 (Seq.fromList [92, 75, 62]) (^ 2) (divisibleBy 3) 3 4

m6 = Monkey 6 (Seq.fromList [94, 86, 76, 67]) (+ 1) (divisibleBy 11) 5 2

m7 = Monkey 7 (Seq.fromList [57]) (+ 2) (divisibleBy 17) 6 2

day11 = V.fromList [m0, m1, m2, m3, m4, m5, m6, m7]

-- Part 2 - chinese remainder theorem to the rescue
lcmOfPrimes = 17 * 11 * 3 * 5 * 2 * 19 * 7 * 13

manageWorry i = i `mod` lcmOfPrimes

solve11B monkeylist =
  (product . take 2 . reverse . sort . V.toList) $
    nRoundsWithCount manageWorry 10000 monkeylist
