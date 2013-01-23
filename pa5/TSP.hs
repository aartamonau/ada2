-- module Main (main) where

import Control.Monad (when)

import Data.List (foldl')
import Data.Bits ((.|.), xor, bitSize, shiftL)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import System.Environment (getArgs)

type City = (Float, Float)
type TSP = (Int, Vector (Vector Float))

type BitSet = Int

empty :: BitSet
empty = 0

singleton :: Int -> BitSet
singleton = fromList . (:[])

insert :: Int -> BitSet -> BitSet
insert x set = (1 `shiftL` x) .|. set

delete :: Int -> BitSet -> BitSet
delete x set = (1 `shiftL` x) `xor` set

-- member :: Int -> BitSet -> Bool
-- member x set = ((1 `shiftL` x) .&. set) /= 0

fromList :: [Int] -> BitSet
fromList = foldl' (flip insert) empty

-- toList :: BitSet -> [Int]
-- toList set = [x | x <- [0 .. bitSize set - 1], x `member` set]

subsets :: Int -> [a] -> [[a]]
subsets 0 [] = [[]]
subsets _ [] = []
subsets i (x : xs) = map (x:) (subsets (i - 1) xs) ++ subsets i xs

subsets1 :: Int -> [a] -> [[a]]
subsets1 0 _      = error "subsets1 size cannot be 0"
subsets1 _ []     = []
subsets1 n (x:xs) = map (x:) $ subsets (n - 1) xs

distance :: City -> City -> Float
distance (x1, y1) (x2, y2) = sqrt $ a * a + b * b
  where a = x2 - x1
        b = y2 - y1

readTSP :: FilePath -> IO TSP
readTSP path = do
  contents <- fmap lines (readFile path)
  let n = read (head contents)
  let cities = map readCity (take n $ tail contents)

  when (n > bitSize (undefined :: BitSet)) $
    error "too many cities to store in bitset"

  return (n, Vector.generate n (row cities))

  where readCity :: String -> City
        readCity s = (read x, read y)
          where [x, y] = words s

        row :: [City] -> Int -> Vector Float
        row cs i = Vector.fromList [distance from to | to <- cs]
          where from = cs !! i

solveTSP :: TSP -> Float
solveTSP (n, tsp) = minimum [d + c | j <- [1 .. n - 1],
                                     let Just d = find cities j a,
                                     let c = tsp ! j ! 1]
  where cities :: BitSet
        cities = fromList [0 .. n - 1]

        initial :: IntMap (IntMap Float)
        initial = IntMap.singleton (singleton 0) $ IntMap.singleton 0 0

        a :: IntMap (IntMap Float)
        a = foldl' (flip ($)) initial (map iter [2..n])

        iter :: Int -> IntMap (IntMap Float) -> IntMap (IntMap Float)
        iter m a = IntMap.fromList [ (fromList s,
                                      IntMap.fromList [(j, d) | j <- s,
                                                                Just d <- [f s j]]) |
                                     s <- subsets1 m [0 .. n - 1] ]
          where f s j = maybeMinimum [d + c | k <- s,
                                      k /= j,
                                      Just d <- [find bitset k a],
                                      let c = tsp ! k ! j]
                  where bitset = j `delete` fromList s

        find :: BitSet -> Int -> (IntMap (IntMap Float)) -> Maybe Float
        find s k a = IntMap.lookup s a >>= IntMap.lookup k

        maybeMinimum [] = Nothing
        maybeMinimum xs = Just $ minimum xs

main :: IO ()
main = do
  [path] <- getArgs
  tsp <- readTSP path

  let r = floor (solveTSP tsp) :: Int
  print r
