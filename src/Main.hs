module Main where

import Data.Graph.Inductive.Graph  -- need?
import Data.Graph.Inductive.Tree
import Data.Maybe
import FUtil
import System.Console.GetOpt
import qualified Data.Set as S

data Options = Options {
  }

defOpts = Options {
  }

options :: [OptDescr (Options -> Options)]
options = [
  ]

data TaskNode = TaskNode {
  taskName :: String,
  taskDesc :: String
  }
-- KwlrThan influences ordering in default view
-- (todo)
data Conn = DepsOn | Contains | HasProp | KwlrThan deriving (Eq, Ord, Show)
type Tasks = Gr TaskNode (S.Set Conn)

data TaskLogAction =
  AddTask TaskNode |
  DelTask TaskNode |
  AddConn TaskNode TaskNode Conn |
  DelConn TaskNode TaskNode Conn

tasksFromLog :: [TaskLogAction] -> Tasks
tasksFromLog = foldr tasksAddLogAction empty

tasksDoLogAction :: TaskLogAction -> Tasks -> Tasks
tasksDoLogAction action (AddTask node) = error "todo"
tasksDoLogAction action (DelTask node) = error "todo"
tasksDoLogAction action (AddConn node1 node2 conn) = error "todo"
tasksDoLogAction action (DelConn node1 node2 conn) = error "todo"

tasksEmpty :: Gr [Char] b
ltasksEmpty = mkGraph [(0, "top")] []

addTo :: Node -> TNode -> Tasks -> (Node, Tasks)
addTo pI t g = (i, insEdge (pI, i, S.singleton Contains) $ insNode (i, t) g)      where
  [i] = newNodes 1 g

pretty :: (Graph gr) => Node -> gr [Char] (S.Set Conn) -> String
pretty i g = unlines .
  zipWith (\ n l -> padl '0' 2 (show n) ++ " " ++ l) [0..] $ prettyInd 0 i
  where
  prettyInd ind i =
    -- todo:
    --    80 cols
    --    + for expandable?
    --    hide done
    concatMap (\ (x, conns) -> map (indOf ind ++) $
      if Contains `S.member` conns
        then [fromJust (lab g x)] ++ prettyInd (ind + 1) x
        else []
      ) $ lsuc g i
    where
    indOf ind = if ind == 0 then "" else replicate (2 * (ind - 1)) ' ' ++ "- "

main :: IO ()
main = do
  let
    (n, tasks) = addTo 0 "projects" tasksEmpty
    (n', tasks') = addTo n "programming" tasks
  (opts, args) <- doArgs "" defOpts options
  clrScr
  putStr $ take 100 $ pretty 0 tasks'
