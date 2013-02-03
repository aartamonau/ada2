import System.Environment (getArgs)

import Data.Graph (Graph)
import qualified Data.Graph as Graph
import qualified Data.Tree as Tree

import qualified Data.IntSet as IntSet

type SAT2 = Graph

readSAT2 :: FilePath -> IO SAT2
readSAT2 path = do
  contents <- fmap lines $ readFile path
  let n = read $ head contents
  let graph = Graph.buildG (-n, n) $ concatMap readClause (tail contents)
  return graph

  where readClause str = [(-a', b'), (-b', a')]
          where [a, b] = words str
                a' = read a
                b' = read b

satisfiable :: SAT2 -> Bool
satisfiable sat = all satComponent components
  where components = map Tree.flatten $ Graph.scc sat

        satComponent comp = go comp IntSet.empty
          where go [] _ = True
                go (x:xs) s | IntSet.member (-x) s = False
                            | otherwise = go xs (IntSet.insert x s)

main :: IO ()
main = do
  [path] <- getArgs
  sat <- readSAT2 path

  if satisfiable sat
    then putStrLn "satisfiable"
    else putStrLn "unsatisfiable"
