module Main where

import Control.Applicative hiding (empty)
import Control.Arrow
import Control.Monad.Random
import Data.Char
import Data.Graph.Inductive
import Data.List
import Data.Maybe
import FUtil
import Numeric
import System.Console.GetOpt
import System.Directory
import System.FilePath
import qualified Data.Set as S

data Options = Options {
  }

defOpts = Options {
  }

options :: [OptDescr (Options -> Options)]
options = [
  ]

data Guid = Guid Integer
  deriving (Eq, Ord)

instance Show Guid where
  show (Guid i) = (\ s -> replicate (32 - length s) '0' ++ s) $ showHex i ""

instance Read Guid where
  readsPrec _ = map (first Guid) . readHex . dropWhile isSpace

data Task = Task {
  taskName :: String,
  taskDesc :: String,
  taskGuid :: Guid}
  deriving (Eq, Ord, Read, Show)
-- KwlrThan influences ordering in default view
-- (todo)
data Conn = DepsOn | Contains | HasProp | KwlrThan
  deriving (Eq, Ord, Read, Show)

type Tasks = Gr Task (S.Set Conn)
type SRand = Rand StdGen

data TaskLogAction =
  AddTask Guid Task |
  DelTask Guid |
  AddConn Guid Guid Conn |
  DelConn Guid Guid Conn
  deriving (Eq, Ord, Read, Show)

tasksFromLog :: [TaskLogAction] -> Tasks
tasksFromLog = foldl' (flip tasksDoLogAction) tasksEmpty

tasksDoLogAction :: TaskLogAction -> Tasks -> Tasks
tasksDoLogAction (AddTask guid task) = addTask guid task
tasksDoLogAction (DelTask guid) = delTask guid
tasksDoLogAction (AddConn guid1 guid2 conn) = addConn guid1 guid2 conn
tasksDoLogAction (DelConn guid1 guid2 conn) = delConn guid1 guid2 conn

tasksEmpty :: Tasks
tasksEmpty = mkGraph [(0, Task "top" "" $ Guid 0)] []

addTask :: Guid -> Task -> Tasks -> Tasks
addTask guid task tasks =
  insEdge (findNode guid tasks, i, S.singleton Contains) $
  insNode (i, task) tasks
  where
  [i] = newNodes 1 tasks

delTask :: Guid -> Tasks -> Tasks
delTask guid tasks = delNode (findNode guid tasks) tasks

addConn guid1 guid2 conn tasks =
  insEdge (findNode guid1 tasks, findNode guid2 tasks,
  S.insert conn $ findEdge guid1 guid2 tasks) tasks

delConn guid1 guid2 conn tasks =
  insEdge (findNode guid1 tasks, findNode guid2 tasks,
  S.delete conn $ findEdge guid1 guid2 tasks) tasks

findNode guid tasks = node where
  node:_ = map fst . filter ((== guid) . taskGuid . snd) $ labNodes tasks

findEdge :: Guid -> Guid -> Tasks -> S.Set Conn
findEdge guid1 guid2 tasks = maybe S.empty (\ (n1, n2, c) -> c) . listToMaybe .
  filter (\ (n1, n2, c) -> n1 == findNode guid1 tasks &&
                           n2 == findNode guid2 tasks) $ labEdges tasks

pretty :: Guid -> Tasks -> String
pretty guid g = unlines .
  zipWith (\ n l -> padl '0' 2 (show n) ++ " " ++ l) [0..] $ prettyInd 0 i
  where
  i = findNode guid g
  prettyInd ind i =
    -- todo:
    --    80 cols
    --    + for expandable?
    --    hide done
    concatMap (\ (x, conns) -> map (indOf ind ++) $
      if Contains `S.member` conns
        then [taskName . fromJust $ lab g x] ++ prettyInd (ind + 1) x
        else []
      ) $ lsuc g i
    where
    indOf ind = if ind == 0 then "" else replicate (2 * (ind - 1)) ' ' ++ "- "

newGuid :: SRand Guid
newGuid = Guid <$> getRandomR (0, 2 ^ 128)

newTask :: String -> String -> SRand Task
newTask name desc = Task name desc <$> newGuid

save :: (Graph gr) => gr Task (S.Set Conn) -> String
save tasks = nodesStr ++ edgesStr where
  nodesStr = unlines . sort $ map showNode nodes
  edgesStr = unlines . sort $ map showEdge edges
  showNode (_, Task name desc guid) =
    show guid ++ " " ++ show name ++ " " ++ show desc
  showEdge (x, y, c) =
    show (fromJust $ lookup x nodeGuids) ++ " " ++
    show (fromJust $ lookup y nodeGuids) ++ " " ++ show c
  nodeGuids = map (second taskGuid) nodes
  nodes = labNodes tasks
  edges = labEdges tasks

main :: IO ()
main = do
  home <- getHomeDirectory
  let taskLogFilename = home </> ".tondo" </> "taskLog"
  taskLog <- doesFileExist taskLogFilename >>= \ t -> if t
    then map read . lines <$> readFileStrict taskLogFilename
    else do
      -- starter example
      t1 <- evalRandIO $ newTask "projects" ""
      t2 <- evalRandIO $ newTask "programming" ""
      return [
        AddTask (Guid 0) t1,
        AddTask (taskGuid t1) t2]
  let tasks = tasksFromLog taskLog
  putStrLn $ save tasks
  let taskLog' = taskLog
  writeFile taskLogFilename . unlines $ map show taskLog'
