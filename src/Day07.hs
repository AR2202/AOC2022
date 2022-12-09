module Day07
  ( input7
  , print7
  , solve7A
  ) where

import           Common
import           Data.List (foldl', nub)

-----Types -------
data Directory
  = File Int
  | Dir [Directory]
  deriving (Show, Read, Eq)

data TerminalOutput
  = Command TerminalCommand
  | Filesize Int String
  | Directoryname String
  deriving (Show, Read, Eq)

data TerminalCommand
  = DirUp
  | DirDown String
  | Ls
  deriving (Show, Read, Eq)

data Dir
  = Expanded String [Dir]
  | SimpleFile Int String
  | Unexpanded String
  deriving (Show, Read, Eq)

----Part1------
directorySize :: Directory -> Int
directorySize (File i) = i
directorySize (Dir ds) = sum $ map directorySize ds

dirSize (Unexpanded s)    = 0
dirSize (SimpleFile i s)  = i
dirSize (Expanded s dirs) = sum $ map dirSize dirs

dirSizes (Unexpanded s) = []
dirSizes (SimpleFile i s) = []
dirSizes (Expanded s dirs) =
  dirSize (Expanded s dirs) : (concatMap dirSizes dirs)

---parsing-----
parseTerminalOutput :: String -> TerminalOutput
parseTerminalOutput s
  | head s == '$' = Command $ parseTerminalCommand (tail s)
  | (head . words) s == "dir" = Directoryname ((concat . tail . words) s)
  | otherwise = Filesize ((read . head . words) s) ((concat . tail . words) s)

parseTerminalCommand :: String -> TerminalCommand
parseTerminalCommand " cd .." = DirUp
parseTerminalCommand " ls"    = Ls
parseTerminalCommand s        = DirDown (drop 4 s)

executeCommand :: TerminalOutput -> (String, Int, Dir) -> (String, Int, Dir)
executeCommand (Command DirUp) (s, i, d) = (findUpper s (i - 1) d, i - 1, d)
executeCommand (Command Ls) (s, i, d) = (s, i, d)
executeCommand (Command (DirDown name)) (s, i, d) = (name, i + 1, d)
executeCommand (Filesize int name) (s, i, d) =
  (s, i, addTo s i d (SimpleFile int name))
executeCommand (Directoryname name) (s, i, d) =
  (s, i, addTo s i d (Unexpanded name))

addTo :: String -> Int -> Dir -> Dir -> Dir
addTo _ i (SimpleFile int name) _ = SimpleFile int name
addTo s 0 (Unexpanded name) d
  | name == s = Expanded name [d]
  | otherwise = Unexpanded name
addTo s i (Unexpanded name) d = Unexpanded name
addTo s 0 (Expanded name dirs) d
  | name == s = Expanded name (nub (d : dirs))
  | otherwise = Expanded name dirs
addTo s i (Expanded name dirs) d =
  Expanded name (map (flip (addTo s (i - 1)) d) dirs)

findUpper :: String -> Int -> Dir -> String
findUpper name i (Unexpanded _) = ""
findUpper name i (SimpleFile _ _) = ""
findUpper name 0 (Expanded s dirs)
  | dirs `containsDir` name = s
  | otherwise = ""
findUpper name i (Expanded s dirs) = concatMap (findUpper name (i - 1)) dirs

containsDir list name = name `elem` (map toName list)

toName :: Dir -> String
toName (Expanded name _)   = name
toName (SimpleFile i name) = name
toName (Unexpanded name)   = name

input7 filename = map parseTerminalOutput . tail . lines <$> loadInput filename

print7 filename = parseTerminalOutput . head . lines <$> loadInput filename

solve7A filename = do
  instructions <- input7 filename
  let initial = ("/", 0, Unexpanded "/")
  let (n, l, test) = foldl' (flip executeCommand) initial (take 20 instructions)
  let testsize = dirSizes test
  let (name, level, dirTree) = foldl' (flip executeCommand) initial instructions
  let dirsizes = dirSizes dirTree
  let dirsizesum = sum $ filter (<= 100000) dirsizes
  print level
  print dirTree
  print dirsizes
  print dirsizesum
