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
  = Expanded String Int [Dir]
  | SimpleFile Int Int String
  | Unexpanded String Int
  deriving (Show, Read, Eq)

data Filetype
  = Directory
  | JustFile
  deriving (Show, Read, Eq)

----Part1------
directorySize :: Directory -> Int
directorySize (File i) = i
directorySize (Dir ds) = sum $ map directorySize ds

dirSize (Unexpanded s id)    = 0
dirSize (SimpleFile i id s)  = i
dirSize (Expanded s id dirs) = sum $ map dirSize dirs

dirSizes (Unexpanded s id) = []
dirSizes (SimpleFile i id s) = []
dirSizes (Expanded s id dirs) =
  dirSize (Expanded s id dirs) : (concatMap dirSizes dirs)

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

executeCommand :: TerminalOutput -> (Int, Int, Int, Dir) -> (Int, Int, Int, Dir)
executeCommand (Command DirUp) (id, maxid, i, d) =
  (head $ findUpper id (i - 1) d, maxid, i - 1, d)
executeCommand (Command Ls) (id, maxid, i, d) = (id, maxid, i, d)
executeCommand (Command (DirDown name)) (id, maxid, i, d) =
  (head $findId name id d, maxid, i + 1, d)
executeCommand (Filesize int name) (id, maxid, i, d) =
  (id, maxid + 1, i, addTo id i d (SimpleFile int (maxid + 1) name))
executeCommand (Directoryname name) (id, maxid, i, d) =
  (id, maxid + 1, i, addTo id i d (Unexpanded name (maxid + 1)))

addTo :: Int -> Int -> Dir -> Dir -> Dir
addTo _ i (SimpleFile int id name) _ = SimpleFile int id name
addTo id 0 (Unexpanded name dirid) d
  | id == dirid = Expanded name dirid [d]
  | otherwise = Unexpanded name dirid
addTo id i (Unexpanded name dirid) d = Unexpanded name dirid
addTo id 0 (Expanded name dirid dirs) d
  | dirid == id = Expanded name dirid (nub (d : dirs))
  | otherwise = Expanded name dirid dirs
addTo id i (Expanded name dirid dirs) d =
  Expanded name dirid (map (flip (addTo id (i - 1)) d) dirs)

findUpper :: Int -> Int -> Dir -> [Int]
findUpper id i (Unexpanded _ _) = []
findUpper id i (SimpleFile _ _ _) = []
findUpper id 0 (Expanded s dirid dirs)
  | dirs `containsDir` id = [dirid]
  | otherwise = []
findUpper id i (Expanded s dirid dirs) = concatMap (findUpper id (i - 1)) dirs

containsDir list name = name `elem` (map toId list)

toId :: Dir -> Int
toId (Expanded name dirid _)   = dirid
toId (SimpleFile i dirid name) = dirid
toId (Unexpanded name dirid)   = dirid

findId :: String -> Int -> Dir -> [Int]
findId dirname parentId (Unexpanded _ _) = []
findId dirname paretnId (SimpleFile _ _ _) = []
findId dirname parentId (Expanded s dirid dirs)
  | parentId == dirid =
    map toId . filter (\dir -> dirName dir == dirname) $ dirs
  | otherwise = concatMap (findId dirname parentId) dirs

dirName :: Dir -> String
dirName (Expanded name _ _)       = name
dirName (SimpleFile i dirid name) = name
dirName (Unexpanded name dirid)   = name

input7 filename = map parseTerminalOutput . tail . lines <$> loadInput filename

print7 filename = parseTerminalOutput . head . lines <$> loadInput filename

solve7A filename = do
  instructions <- input7 filename
  let initial = (0, 0, 0, Unexpanded "/" 0)
  let (id, maxid, level, dirTree) =
        foldl' (flip executeCommand) initial instructions
  let dirsizes = dirSizes dirTree
  let dirsizesum = sum $ filter (<= 100000) dirsizes
  print dirsizes
  print dirsizesum
