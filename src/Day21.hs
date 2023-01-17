module Day21
  ( example21
  , parsetest
  , parsetestadd
  , parsetestequation
  , parseEquationsFromFile
  , makeVarDict
  , testMakeMatrixRow
  , testMakeMatrixRowMult
  , testMakeMatrixRowDiv
  , testMakeMatrixRows
  , testMakeRHS
  , testMakematrix
  , testsolveMatrix
  , solve21A
  , solve21A'
  ) where

import           Common
import qualified Data.Map                                 as M
import           Numeric.LinearAlgebra
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator

--------Types---------
data Expression
  = Num Int
  | Add String String
  | Mul String String
  | Sub String String
  | Div String String
  deriving (Show, Read, Eq)

type Equation = (String, Expression)

--------Solution------
------Part1-------
solve21A filename = fmap solveRoot <$> parseEquationsFromFile filename

solve21A' filename =
  fmap (evalRoot . M.fromList) <$> parseEquationsFromFile filename

------Parsing input---------
varNameParser :: Parser String
varNameParser = concat <$> (many1 alphaNum) `endBy` (string ":")

additionParser :: Parser Expression
additionParser = do
  arg1 <- many1 alphaNum
  _ <- spaces
  _ <- char '+'
  _ <- spaces
  arg2 <- many1 alphaNum
  return (Add arg1 arg2)

subtractionParser :: Parser Expression
subtractionParser = do
  arg1 <- many1 alphaNum
  _ <- spaces
  _ <- char '-'
  _ <- spaces
  arg2 <- many1 alphaNum
  return (Sub arg1 arg2)

multiplicationParser :: Parser Expression
multiplicationParser = do
  arg1 <- many1 alphaNum
  _ <- spaces
  _ <- char '*'
  _ <- spaces
  arg2 <- many1 alphaNum
  return (Mul arg1 arg2)

divisionParser :: Parser Expression
divisionParser = do
  arg1 <- many1 alphaNum
  _ <- spaces
  _ <- char '/'
  _ <- spaces
  arg2 <- many1 alphaNum
  return (Div arg1 arg2)

varNameAddParser :: Parser Equation
varNameAddParser = do
  name <- varNameParser
  _ <- spaces
  exp <- additionParser
  return (name, exp)

varNameExpParser :: Parser Equation
varNameExpParser = do
  name <- varNameParser
  _ <- spaces
  exp <-
    try additionParser <|> try subtractionParser <|> try multiplicationParser <|>
    try divisionParser <|>
    numparser
  return (name, exp)

numparser :: Parser Expression
numparser = Num . read <$> (many1 digit)

parseEquationsFromFile filename =
  sequenceA . map (parse varNameExpParser filename) <$> splitLines filename

-------evaluation------
eval :: String -> M.Map String Expression -> Int
eval s m =
  case evalVar s m of
    Num c   -> c
    Add a b -> eval a m + eval b m
    Sub a b -> eval a m - eval b m
    Mul a b -> eval a m * eval b m
    Div a b -> eval a m `div` eval b m

evalRoot = eval "root"

--------convoluted attempt to parse as a matrix----------
makeVarDict filename = fmap vardict <$> parseEquationsFromFile filename

vardict = M.fromList . (flip zip) [1 ..] . map fst

evalVar :: String -> M.Map String Expression -> Expression
evalVar = M.findWithDefault (Num 0)

findRoot = M.findWithDefault 0 "root"

evalMult :: (String, String) -> M.Map String Expression -> (Int, String)
evalMult (a, b) expressionmap =
  case evalVar a expressionmap of
    Num c -> (c, b)
    _ ->
      case evalVar b expressionmap of
        Num c -> (c, a)
        _     -> (0, a)

evalDiv :: (String, String) -> M.Map String Expression -> (Int, String)
evalDiv (a, b) expressionmap =
  case evalVar b expressionmap of
    Num c -> (c, a)
    _     -> (0, a)

makeMatrixRow ::
     M.Map String Int -> M.Map String Expression -> Equation -> [Double]
makeMatrixRow vars _ (varname, Num a) =
  [ if x == varnum
    then 1.0
    else 0.0
  | x <- [1 .. M.size vars]
  ]
  where
    varnum = M.findWithDefault 0 varname vars
makeMatrixRow vars _ (varname, Add arg1 arg2) =
  [ if x == varnum
    then 1.0
    else if x == arg1num || x == arg2num
           then -1.0
           else 0.0
  | x <- [1 .. M.size vars]
  ]
  where
    varnum = M.findWithDefault 0 varname vars
    arg1num = M.findWithDefault 0 arg1 vars
    arg2num = M.findWithDefault 0 arg2 vars
makeMatrixRow vars _ (varname, Sub arg1 arg2) =
  [ if x == varnum
    then 1.0
    else if x == arg1num
           then -1.0
           else if x == arg2num
                  then 1.0
                  else 0.0
  | x <- [1 .. M.size vars]
  ]
  where
    varnum = M.findWithDefault 0 varname vars
    arg1num = M.findWithDefault 0 arg1 vars
    arg2num = M.findWithDefault 0 arg2 vars
makeMatrixRow vars expressionmap (varname, Mul arg1 arg2) =
  [ if x == varnum
    then 1.0
    else if x == argnum
           then -1.0 * fromIntegral argcoeff
           else 0.0
  | x <- [1 .. M.size vars]
  ]
  where
    varnum = M.findWithDefault 0 varname vars
    argnum = M.findWithDefault 0 arg vars
    (argcoeff, arg) = evalMult (arg1, arg2) expressionmap
makeMatrixRow vars expressionmap (varname, Div arg1 arg2) =
  [ if x == varnum
    then 1.0
    else if x == argnum
           then -1.0 / (fromIntegral argcoeff)
           else 0.0
  | x <- [1 .. M.size vars]
  ]
  where
    varnum = M.findWithDefault 0 varname vars
    argnum = M.findWithDefault 0 arg vars
    (argcoeff, arg) = evalDiv (arg1, arg2) expressionmap

makeRHS (s, Num x) = fromIntegral x
makeRHS _          = 0.0

makeRHSVec equations = vector $ map makeRHS equations

makeMatrixRows :: [Equation] -> [[Double]]
makeMatrixRows equations =
  map (makeMatrixRow (vardict equations) (M.fromList equations)) equations

makeMatrix equations =
  (length equations >< length equations) $ concat $ makeMatrixRows equations

-------solve system of equations------
solveMatrix equations = (makeMatrix equations) <\> (makeRHSVec equations)

solveRoot equations = solveMatrix equations ! (findRoot (vardict equations) - 1)

---------Some tests---------
testMakeMatrixRow =
  makeMatrixRow
    (M.fromList
       [ ("cczh", 3)
       , ("dbpl", 2)
       , ("drzm", 14)
       , ("dvpt", 6)
       , ("hmdt", 15)
       , ("humn", 8)
       , ("lfqf", 7)
       , ("lgvd", 13)
       , ("ljgn", 9)
       , ("pppw", 12)
       , ("ptdq", 5)
       , ("root", 1)
       , ("sjmn", 10)
       , ("sllz", 11)
       , ("zczc", 4)
       ])
    (M.fromList
       [ ("cczh", Add "sllz" "lgvd")
       , ("dbpl", Num 5)
       , ("drzm", Sub "hmdt" "zczc")
       , ("dvpt", Num 3)
       , ("hmdt", Num 32)
       , ("humn", Num 5)
       , ("lfqf", Num 4)
       , ("lgvd", Mul "ljgn" "ptdq")
       , ("ljgn", Num 2)
       , ("pppw", Div "cczh" "lfqf")
       , ("ptdq", Sub "humn" "dvpt")
       , ("root", Add "pppw" "sjmn")
       , ("sjmn", Mul "drzm" "dbpl")
       , ("sllz", Num 4)
       , ("zczc", Num 2)
       ])
    ("root", Add "pppw" "sjmn")

testMakeMatrixRowMult =
  makeMatrixRow
    (M.fromList
       [ ("cczh", 3)
       , ("dbpl", 2)
       , ("drzm", 14)
       , ("dvpt", 6)
       , ("hmdt", 15)
       , ("humn", 8)
       , ("lfqf", 7)
       , ("lgvd", 13)
       , ("ljgn", 9)
       , ("pppw", 12)
       , ("ptdq", 5)
       , ("root", 1)
       , ("sjmn", 10)
       , ("sllz", 11)
       , ("zczc", 4)
       ])
    (M.fromList
       [ ("cczh", Add "sllz" "lgvd")
       , ("dbpl", Num 5)
       , ("drzm", Sub "hmdt" "zczc")
       , ("dvpt", Num 3)
       , ("hmdt", Num 32)
       , ("humn", Num 5)
       , ("lfqf", Num 4)
       , ("lgvd", Mul "ljgn" "ptdq")
       , ("ljgn", Num 2)
       , ("pppw", Div "cczh" "lfqf")
       , ("ptdq", Sub "humn" "dvpt")
       , ("root", Add "pppw" "sjmn")
       , ("sjmn", Mul "drzm" "dbpl")
       , ("sllz", Num 4)
       , ("zczc", Num 2)
       ])
    ("sjmn", Mul "drzm" "dbpl")

testMakeMatrixRowDiv =
  makeMatrixRow
    (M.fromList
       [ ("cczh", 3)
       , ("dbpl", 2)
       , ("drzm", 14)
       , ("dvpt", 6)
       , ("hmdt", 15)
       , ("humn", 8)
       , ("lfqf", 7)
       , ("lgvd", 13)
       , ("ljgn", 9)
       , ("pppw", 12)
       , ("ptdq", 5)
       , ("root", 1)
       , ("sjmn", 10)
       , ("sllz", 11)
       , ("zczc", 4)
       ])
    (M.fromList
       [ ("cczh", Add "sllz" "lgvd")
       , ("dbpl", Num 5)
       , ("drzm", Sub "hmdt" "zczc")
       , ("dvpt", Num 3)
       , ("hmdt", Num 32)
       , ("humn", Num 5)
       , ("lfqf", Num 4)
       , ("lgvd", Mul "ljgn" "ptdq")
       , ("ljgn", Num 2)
       , ("pppw", Div "cczh" "lfqf")
       , ("ptdq", Sub "humn" "dvpt")
       , ("root", Add "pppw" "sjmn")
       , ("sjmn", Mul "drzm" "dbpl")
       , ("sllz", Num 4)
       , ("zczc", Num 2)
       ])
    ("pppw", Div "cczh" "lfqf")

testMakeMatrixRows = makeMatrixRows testexample21

testMakeRHS = makeRHSVec testexample21

testMakematrix = makeMatrix testexample21

testsolveMatrix = solveMatrix testexample21

testexample21 =
  [ ("root", Add "pppw" "sjmn")
  , ("dbpl", Num 5)
  , ("cczh", Add "sllz" "lgvd")
  , ("zczc", Num 2)
  , ("ptdq", Sub "humn" "dvpt")
  , ("dvpt", Num 3)
  , ("lfqf", Num 4)
  , ("humn", Num 5)
  , ("ljgn", Num 2)
  , ("sjmn", Mul "drzm" "dbpl")
  , ("sllz", Num 4)
  , ("pppw", Div "cczh" "lfqf")
  , ("lgvd", Mul "ljgn" "ptdq")
  , ("drzm", Sub "hmdt" "zczc")
  , ("hmdt", Num 32)
  ]

example21A =
  (15 >< 15)
    [ 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , -1.0
    , 0.0
    , -1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , -1.0
    , 0.0
    , -1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 1.0
    , 0.0
    , -1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , -5.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , -0.25
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , -2.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , -1.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    ]

example21Aright =
  vector
    [0.0, 5.0, 0.0, 2.0, 0.0, 3.0, 4.0, 5.0, 2.0, 0.0, 4.0, 0.0, 0.0, 0.0, 32]

example21 = example21A <\> example21Aright

parsetest = parse varNameParser "test"

parsetestadd = parse additionParser "test"

parsetestequation = parse varNameExpParser "test"
