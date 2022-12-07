module Main where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import SantaLib
import SantaLib.Parsing
import System.Exit (exitFailure)

data FileTree = Dir (Map String FileTree) | File Int
  deriving (Show)

-- |  A bread crumb saves which directory we've entered, as well as the other
--  directories and files.
data BreadCrumb = Crumb String (Map String FileTree)
  deriving (Show)

-- | Zipper for File Tree, keeps current directory contents, as well as bread
-- crumbs for path taken
type Location = (Map String FileTree, [BreadCrumb])

data Path = ChildDir String | Root | ToParent
  deriving (Show)

data Cmd = Cd Path | Touch String Int | MkDir String
  deriving (Show)

pPath :: Parser Path
pPath = pRoot <|> pParent <|> pChildDir
  where
    pRoot = Root <$ symbol "/"
    pParent = ToParent <$ symbol ".."
    pChildDir = ChildDir <$> lexeme (some printChar)

pLSResult :: Parser [Cmd]
pLSResult = some $ pDir <|> pFile
  where
    pDir = symbol "dir" >> MkDir <$> lexemeLn (some printChar)
    pFile = do
      f <- lexeme decimal
      n <- lexemeLn (some printChar)
      pure (Touch n f)

pCommand :: Parser [Cmd]
pCommand = symbol "$" >> (pCd <|> pLs)
  where
    pCd = (: []) . Cd <$> (symbol "cd" >> lexemeLn pPath)
    pLs = symbolLn "ls" >> pLSResult

pInp :: Parser [Cmd]
pInp = concat <$> some pCommand

root :: Location
root = (M.empty, [])

cd :: Path -> Location -> Maybe Location
cd Root (dirContents, parents) = case parents of
  [] -> Just (dirContents, [])
  _ -> cd ToParent (dirContents, parents) >>= cd Root
cd ToParent (dirContents, parents) = case parents of
  [] -> Nothing
  (Crumb name parentContents : grandParents) -> Just (M.insert name (Dir dirContents) parentContents, grandParents)
cd (ChildDir name) (dirContents, parents) = do
  (Dir contents) <- dirContents M.!? name
  let dirContents' = M.delete name dirContents
  Just (contents, Crumb name dirContents' : parents)

mkdir :: String -> Location -> Location
mkdir name (dirContents, parents) = (M.insert name (Dir M.empty) dirContents, parents)

touch :: String -> Int -> Location -> Location
touch name size (dirContents, parents) = (M.insert name (File size) dirContents, parents)

walk :: [Cmd] -> Location -> Maybe Location
walk [] location = Just location
walk (MkDir name : rest) location = walk rest $ mkdir name location
walk (Touch name size : rest) location = walk rest $ touch name size location
walk (Cd path : rest) location = do
  ft' <- cd path location
  walk rest ft'

size :: FileTree -> Int
size (Dir contents) = sum (M.map size contents)
size (File s) = s

sizeTable :: FileTree -> String -> Map String Int
sizeTable (File s) name = M.empty
sizeTable (Dir contents) name = M.insert name (sum $ M.map size contents) sizes
  where
    sizes = foldr M.union M.empty sizeTables
    sizeTables = map (\(n, ft) -> sizeTable ft (name <> "/" <> n)) $ M.toList contents

part1 :: [Cmd] -> Int
part1 commands = fromJust $ do
  res <- walk commands root
  (rootContents, []) <- cd Root res
  let table = sizeTable (Dir rootContents) ""
  let smallDirs = M.filter (<= 100000) table
  return (sum smallDirs)

part2 :: [Cmd] -> Int
part2 commands = fromJust $ do
  res <- walk commands root
  (rootContents, []) <- cd Root res
  let table = sizeTable (Dir rootContents) ""
  let totalSize = size (Dir rootContents)
  let sizeNeeded = totalSize - (70000000 - 30000000)
  let viables = M.filter (> sizeNeeded) table
  return (minimum viables)

main :: IO ()
main = do
  inp <- getInput 7
  commands <- case parse pInp "day7.input" inp of
    Left error -> putStrLn (errorBundlePretty error) >> exitFailure
    Right cmds -> return cmds
  putAnswer 7 Part1 (part1 commands)
  putAnswer 7 Part2 (part2 commands)
