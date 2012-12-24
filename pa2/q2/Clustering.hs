{-# LANGUAGE ViewPatterns #-}

import Data.Bits ( xor, shiftL )

import Data.IntSet ( IntSet )
import qualified Data.IntSet as IntSet

import System.Environment ( getArgs )

type BitString = Int

readInput :: FilePath -> IO (Int, Int, [BitString])
readInput path = do
  content <- fmap lines (readFile path)
  let [n, b] = map read $ words (head content)
  let nodes  = map parse $ tail content
  return (n, b, nodes)

  where parse (words -> bs) = goParse bs 0

        goParse [] r = r
        goParse ("1" : bs) r = goParse bs (r * 2 + 1)
        goParse ("0" : bs) r = goParse bs (r * 2)

flipBit :: BitString -> Int -> BitString
flipBit bs p = (1 `shiftL` p) `xor` bs

flipBits :: BitString -> [Int] -> BitString
flipBits bs ps = foldl flipBit bs ps

positions :: Int -> Int -> [[Int]]
positions b k = go k [0 .. b - 1]
  where go 0 _ = [[]]
        go _ [] = []
        go k (x:xs) = map (x:) (go (k - 1) xs) ++ go k xs

neighbors :: Int -> Int -> BitString -> [BitString]
neighbors k b bs = concatMap (map (flipBits bs) . positions b) [1 .. k]

neededClusters :: Int -> (Int, Int, [BitString]) -> Int
neededClusters k (n, b, nodes) = go 0 IntSet.empty nodes
  where go count _ [] = count
        go count visited (n : ns)
          | IntSet.member n visited = go count visited ns
          | otherwise               = go (count + 1) (visit n visited) ns

        nodesSet = IntSet.fromList nodes

        visit n visited = foldl (flip visit) visited' ns
          where visited' = IntSet.insert n visited
                ns       = filter isGood $ neighbors 2 b n
                isGood n = IntSet.member n nodesSet && not (IntSet.member n visited')

main :: IO ()
main = do
  args <- getArgs
  let path | [path'] <- args = path'
           | otherwise       = "clustering2.txt"

  input <- readInput path
  print $ neededClusters 3 input
