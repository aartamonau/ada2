{-# LANGUAGE ViewPatterns #-}

import Data.List (foldl')
import Data.Maybe (fromJust)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Graph.Inductive (Gr, Node, LEdge)
import qualified Data.Graph.Inductive as G

import Data.FingerTree.PSQueue (PSQ, Binding ((:->)))
import qualified Data.FingerTree.PSQueue as PSQ

import System.Environment (getArgs)

data Weight = Finite !Int | Inf
            deriving Eq

instance Read Weight where
  readsPrec p s = [(Finite n, rest)]
    where [(n, rest)] = readsPrec p s

instance Show Weight where
  show (Finite n) = show n
  show Inf        = "inf"

instance Num Weight where
  Finite x + Finite y = Finite (x + y)
  Inf + Finite _ = Inf
  Finite _ + Inf = Inf
  Inf + Inf = Inf

  Finite x - Finite y = Finite (x - y)
  Inf - Finite _ = Inf
  Finite _ - Inf = Inf
  _ - _ = error "unsupported (inf - inf)"

  _ * _ = error "unsupported (*)"

  abs = error "unsupported (abs)"
  signum = error "unsupported (signum)"
  fromInteger = Finite . fromInteger

instance Ord Weight where
  compare (Finite x) (Finite y) = compare x y
  compare Inf (Finite _) = GT
  compare (Finite _) Inf = LT
  compare Inf Inf = EQ

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
        initialPsq = PSQ.fromList [n :-> w | n <- G.nodes g,
                                             let w | s == n = 0
                                                   | otherwise = Inf]

        go :: PSQ Node Weight -> IntMap Weight -> IntMap Weight
        go (PSQ.minView -> Nothing) r = r
        go (PSQ.minView -> Just (cur :-> d, psq')) r = go psq'' r'
          where adjust new old = Just $ min new old
                out = [assertError (w >= 0) "got negative weight edge" (v, w) |
                       (_, v, w) <- G.out g cur]
                k p (v, w) = PSQ.update (adjust (w + d)) v p

                psq'' = foldl' k psq' out
                r' = IntMap.insert cur d r

bellmanFord :: Gr a Weight -> Node -> IntMap Weight
bellmanFord g s = go 1 initialDs
  where initialDs = IntMap.fromList initialList
        initialList = [(n, w) | n <- G.nodes g, let w | n == s = 0
                                                      | otherwise = Inf]

        find k = fromJust . IntMap.lookup k
        nodesCount = G.noNodes g

        iter ds = IntMap.mapWithKey update ds
          where update n d = minimum (d : inn)
                  where inn = [w + ud | (u, _, w) <- G.inn g n,
                               let ud = find u ds]

        go i ds
          | i == nodesCount = assertError (ds == ds')
                                          "the graph has negative weight loops"
                                          ds
          | ds == ds' = ds
          | otherwise = go (i + 1) ds'
            where ds' = iter ds

johnsonReweight :: Gr a Weight -> (Gr a Weight, IntMap Weight)
johnsonReweight g = (G.gmap mapContext g, IntMap.delete fakeNode ps)
  where (_, maxNode) = G.nodeRange g
        fakeNode = maxNode + 1
        nodes = G.nodes g
        fakeEdges = [(fakeNode, n, 0) | n <- nodes]
        g' = G.insEdges fakeEdges (G.insNode (fakeNode, undefined) g)

        ps = bellmanFord g' fakeNode
        mapWeight (u, v, w) = w + pu - pv
          where pu = fromJust $ IntMap.lookup u ps
                pv = fromJust $ IntMap.lookup v ps

        mapContext (inn, n, l, out) = (inn', n, l, out')
          where inn' = [(mapWeight (u, n, w), u) | (w, u) <- inn]
                out' = [(mapWeight (n, v, w), v) | (w, v) <- out]

johnson :: Gr a Weight -> IntMap (IntMap Weight)
johnson g = IntMap.fromList [(n, unreweight n (dijkstra g' n)) | n <- G.nodes g]
  where (g', ps) = johnsonReweight g
        unreweight u = IntMap.mapWithKey k
          where k v w = w - pu + pv
                  where pv = fromJust $ IntMap.lookup v ps
                pu = fromJust $ IntMap.lookup u ps

floydWarshall :: Gr a Weight -> IntMap (IntMap Weight)
floydWarshall g = go nodes initial'
  where nodes = G.nodes g
        edges = G.labEdges g

        insert (u, v, w) = IntMap.insertWith nested u def
          where nested _ = IntMap.insert v w
                def = IntMap.singleton v w

        lookup (u, v) m = fromJust $ IntMap.lookup v (fromJust (IntMap.lookup u m))

        initial = foldl' (flip insert) IntMap.empty
                         [(u, v, w) | u <- nodes, v <- nodes,
                                      let w | u == v = 0
                                            | otherwise = Inf]
        initial' = foldl' (flip insert) initial edges

        checkLoops m = assertError noLoops "the graph has negative weight loops" m
          where noLoops = all (>=0) (map (`lookup` m) [(n, n) | n <- nodes])

        iter k m = foldl' (flip insert) IntMap.empty new
          where new = [(u, v, newW) | u <- nodes, v <- nodes,
                                      let oldW = lookup (u, v) m,
                                      let wuk  = lookup (u, k) m,
                                      let wkv  = lookup (k, v) m,
                                      let altW = wuk + wkv,
                                      let newW = min oldW altW]

        go [] m = checkLoops m
        go (n : ns) m = go ns (iter n m)

algo :: String -> (Gr a Weight -> IntMap (IntMap Weight))
algo "fw" = floydWarshall
algo "johnson" = johnson

main :: IO ()
main = do
  [algoStr, path] <- getArgs
  graph <- readGraph path

  let ds = algo algoStr graph
  let md = minD $ IntMap.map minD ds

  print md

  where minD = IntMap.fold min Inf
