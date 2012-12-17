import Data.List ( minimumBy )

import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet

import Data.Function ( on )

import Data.Graph.Inductive ( Gr, LEdge, Context, Node,
                              insNodes, insEdges, empty, out', inn',
                              labEdges )

type Weight = Int
type WGraph = Gr () Weight

readGraph :: IO WGraph
readGraph = do
  content <- fmap lines (readFile "edges.txt")
  let [vertexCount, _] = map read $ words $ head content
  let vertices         = [(v, ()) | v <- [1 .. vertexCount]]
  let initialGraph     = insNodes vertices empty
  let edges            = map parseEdge $ tail content

  return $ insEdges edges initialGraph

  where parseEdge :: String -> LEdge Weight
        parseEdge s = let [a, b, w] = words s
                      in (read a, read b, read w)

mstWeight :: WGraph -> Weight
mstWeight graph = go (IntSet.singleton 1) 0
  where go cut w | [] <- crossEdges = w
                 | otherwise        = go (IntSet.union cut nextEdgeVertices)
                                         (w + nextEdgeWeight)
          where crossEdges        = filter (isCrossEdge cut) graphEdges
                nextEdge          = minimumBy (compare `on` labEdge) crossEdges
                labEdge (_, _, l) = l

                (nextEdgeV, nextEdgeW, nextEdgeWeight) = nextEdge
                nextEdgeVertices = IntSet.fromList [nextEdgeV, nextEdgeW]

        ctxEdges :: Context a b -> [LEdge b]
        ctxEdges ctx = out' ctx ++ inn' ctx

        isCrossEdge :: IntSet -> LEdge b -> Bool
        isCrossEdge cut (v, w, _) =
          (IntSet.member v cut && IntSet.notMember w cut) ||
          (IntSet.member w cut && IntSet.notMember v cut)

        graphEdges :: [LEdge Weight]
        graphEdges = labEdges graph

main :: IO ()
main = do
  graph <- readGraph
  print $ mstWeight graph
