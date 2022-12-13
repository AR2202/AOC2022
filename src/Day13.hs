module Day13
  ( nodeParser
  , treeParser
  , leafParser
  , parseAsTree
  , input13
  , day13A
  ) where

import           Common
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

import           Data.Either                              (fromRight)
import           Text.ParserCombinators.Parsec.Char
import           Text.ParserCombinators.Parsec.Combinator

----Types -----
data ListTree
  = Leaf Int
  | Node [ListTree]
  deriving (Show, Read, Eq)

data ListOrder
  = Correct
  | Incorrect
  | Undecidable
  deriving (Show, Read, Eq)

---Part 1--------
compareTrees :: [ListTree] -> [ListTree] -> ListOrder
compareTrees (Leaf i:xs) (Leaf j:ys)
  | i < j = Correct
  | i > j = Incorrect
  | i == j = compareTrees xs ys
compareTrees (Node l:xs) (Leaf j:ys) =
  compareTrees (Node l : xs) (Node [Leaf j] : ys)
compareTrees (Leaf j:ys) (Node l:xs) =
  compareTrees (Node [Leaf j] : ys) (Node l : xs)
compareTrees [] [] = Undecidable
compareTrees [] xs = Correct
compareTrees xs [] = Incorrect
compareTrees (Node []:xs) (Node []:ys) = compareTrees xs ys
compareTrees (Node []:xs) (Node zs:ys) = Correct
compareTrees (Node zs:xs) (Node []:ys) = Incorrect
compareTrees (Node as:xs) (Node bs:ys) =
  case compareTrees as bs of
    Undecidable -> compareTrees xs ys
    decision    -> decision

compareTrees' :: ListTree -> ListTree -> ListOrder
compareTrees' t1 t2 = compareTrees [t1] [t2]

--Parsing----
nodeParser :: Parser ListTree
nodeParser = Node <$> between (char '[') (char ']') (many treeParser)

nodeParserList :: Parser ListTree
nodeParserList =
  Node <$> (char ',' *> between (char '[') (char ']') (many treeParser))

nodeParserList2 :: Parser ListTree
nodeParserList2 =
  Node <$> (between (char '[') (char ']') (many treeParser) <* char ',')

leafParser :: Parser ListTree
leafParser = Leaf . read <$> (many1 digit <* char ',')

leafParserEnd :: Parser ListTree
leafParserEnd = Leaf . read <$> (many1 digit)

treeParser :: Parser ListTree
treeParser =
  try leafParser <|> try nodeParserList <|> try nodeParserList2 <|>
  try nodeParser <|>
  try leafParserEnd

parseAsTree :: String -> Either ParseError ListTree
parseAsTree = parse treeParser "test"

----load input------
input13 :: String -> IO Int
input13 filename =
  sum .
  map fst .
  filter (\x -> (snd x) == Correct) .
  (zip [1 ..]) .
  map (uncurry compareTrees' . list2tuple) .
  (map . map) (fromRight (Node []) . parseAsTree) . map lines <$>
  splitOnBlankLine filename

day13A :: IO Int
day13A = input13 "Day13.txt"
