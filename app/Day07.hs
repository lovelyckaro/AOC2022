module Main where
import SantaLib
import SantaLib.Parsing
import Data.Map qualified as M
import Data.Map (Map)
import Data.Ord
import Data.Maybe
import System.Exit (exitFailure)


data FileTree = Dir (Map String FileTree) | File Int
  deriving Show

data BreadCrumb = Crumb String (Map String FileTree)
  deriving Show

type FileTreeZipper = (Map String FileTree, [BreadCrumb])

data Path = ChildDir String | Root | ToParent
  deriving Show

data Cmd = Cd Path | Ls (Map String FileTree)
  deriving Show

pPath :: Parser Path
pPath = pRoot <|> pParent <|> pChildDir
  where
    pRoot = Root <$ symbol "/"
    pParent = ToParent <$ symbol ".."
    pChildDir = ChildDir <$> lexeme (some printChar)

pLSResult :: Parser (Map String FileTree)
pLSResult = do
  results <- some $ pDir <|> pFile
  return $ M.fromList results
  where
    pDir = do
      symbol "dir"
      n <- lexemeLn (some printChar)
      pure (n, Dir M.empty)
    pFile = do
      f <- File <$> lexeme decimal
      n <- lexemeLn (some printChar)
      pure (n, f)

pCommand :: Parser Cmd
pCommand = symbol "$" >> (pCd <|> pLs)
  where
    pCd = Cd <$> (symbol "cd" >> lexemeLn pPath)
    pLs = Ls <$> (symbolLn "ls" >> pLSResult)

pInp :: Parser [Cmd]
pInp = some pCommand

root :: FileTreeZipper
root = (M.empty, [])

cd :: Path -> FileTreeZipper -> Maybe FileTreeZipper
cd Root (dirContents, parents) = case parents of
  [] -> Just (dirContents, [])
  _ -> let Crumb _ rootContents = last parents in Just (rootContents, [])
cd ToParent (dirContents, parents) = case parents of
  [] -> Nothing
  (Crumb _ parentContents : grandParents) -> Just (parentContents, grandParents)
cd (ChildDir name) (dirContents, parents) = do
  (Dir contents) <- dirContents M.!? name
  Just (contents, Crumb name dirContents : parents)

update :: Map String FileTree -> [BreadCrumb] -> Maybe [BreadCrumb]
update m [] = Just []
update newContents (Crumb chosen parentContents : crumbs) = do
  (Dir knownContents) <- parentContents M.!? chosen
  let mergedContents = M.union newContents knownContents
  let crumb'@(Crumb _ pContents') = Crumb chosen (M.insert chosen (Dir mergedContents) parentContents)
  crumbs' <- update pContents' crumbs
  return (crumb' : crumbs')

walk :: [Cmd] -> FileTreeZipper -> Maybe FileTreeZipper
walk [] zipper = Just zipper
walk (Ls dirContents : rest) (knownContents, parents) = do
    let contents' = M.union dirContents knownContents
    parents' <- update contents' parents
    walk rest (contents', parents')
walk (Cd path : rest) ft = do
  ft' <- cd path ft
  walk rest ft'

size :: FileTree -> Int
size (Dir contents) = sum (M.map size contents)
size (File s) = s

sizeTable :: FileTree -> String ->  Map String Int
sizeTable (File s) name = M.empty
sizeTable (Dir contents) name = M.insert name (sum $ M.map size contents) sizes
  where
    sizes = foldr M.union M.empty sizeTables
    sizeTables = map (\(n, ft) -> sizeTable ft (name <> "/" <> n)) $ M.toList contents 

part1 :: [Cmd] -> Int
part1 commands = fromJust $ do
  res <- walk commands root
  (rootContents,[]) <- cd Root res
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
