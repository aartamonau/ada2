{-# LANGUAGE ViewPatterns  #-}

import Control.Arrow ( first )
import Control.Monad.ST ( ST, runST )

import Data.Array ( array, (!) )
import Data.List ( sortBy )
import Data.Function ( on )
import Data.UnionFind.ST ( fresh, equivalent, union )

data UnionFind = UnionFind

type Vertex = Int
type Edge = (Vertex, Vertex)
type Distance = Int

readInput :: IO (Int, [(Edge, Distance)])
readInput = do
  content <- fmap lines (readFile "clustering1.txt")
  let count = read (head content)
  return $ (count, map parse (tail content))

  where parse :: String -> (Edge, Distance)
        parse (words -> [x, y, w]) = ((read x, read y), read w)


maxSpacing :: Int -> Int -> [(Edge, Distance)] -> Distance
maxSpacing k count (sortBy (compare `on` snd) -> es) = runST go
  where go = do
          sets <- fmap (array (1, count) . zip [1 .. ]) $ mapM fresh [1 .. count]
          let v2p        = (sets !)
          let e2p (x, y) = (v2p x, v2p y)
          let edges      = map (first e2p) es

          goMerge count edges

        goMerge count edges@(((x, y), d) : es)
          | count == k = computeSpacing edges
          | otherwise  = do
            eq <- equivalent x y
            if eq
              then goMerge count es
              else do
                union x y
                goMerge (count - 1) es

        computeSpacing (((x, y), d) : es) = do
          eq <- equivalent x y
          if eq
            then computeSpacing es
            else return d

main :: IO ()
main = do
  (count, edges) <- readInput
  let spacing = maxSpacing 4 count edges
  print spacing
