{-# LANGUAGE ViewPatterns #-}

import Data.List (foldl')

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Graph.Inductive (Gr, Node, LEdge)
import qualified Data.Graph.Inductive as G

import Data.FingerTree.PSQueue (PSQ, Binding ((:->)))
import qualified Data.FingerTree.PSQueue as PSQ

type Weight = Int

assertError :: Bool -> String -> a -> a
assertError True _ v = v
assertError False msg _ = error msg

readGraph :: FilePath -> IO (Gr () Weight)
readGraph path = do
  content <- fmap lines $ readFile path
  let [vertexCount, _] = map read $ words $ head content
  let vertices         = [(v, ()) | v <- [1 .. vertexCount]]
  let initialGraph     = G.insNodes vertices G.empty
  let edges            = map parseEdge $ tail content

  return $ G.insEdges edges initialGraph

  where parseEdge :: String -> LEdge Weight
        parseEdge s = let [a, b, w] = words s
                      in (read a, read b, read w)

dijkstra :: Gr a Weight -> Node -> IntMap Weight
dijkstra g s = go initialPsq IntMap.empty
  where initialPsq :: PSQ Node Weight
        initialPsq = PSQ.fromList [(n :-> w) | n <- G.nodes g,
                                               let w | s == n = 0
                                                     | otherwise = maxBound]

        go :: PSQ Node Weight -> IntMap Weight -> IntMap Weight
        go (PSQ.minView -> Nothing) r = r
        go (PSQ.minView -> Just (cur :-> d, psq')) r = go psq'' r'
          where adjust new old | old <= new = Nothing
                               | otherwise  = Just new

                out = [assertError (w >= 0) "got negative weight edge" (v, w) |
                       (_, v, w) <- G.out g cur]
                k p (v, w) = PSQ.update (adjust (w + d)) v p

                psq'' = foldl' k psq' out
                r' = (IntMap.insert cur d r)
        go (PSQ.minView -> Just (_, _)) _ = undefined

main :: IO ()
main = undefined
