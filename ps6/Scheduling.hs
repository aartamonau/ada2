import Control.Monad (replicateM)
import Data.List (sortBy, maximumBy)
import Data.Function (on)
import System.Random (Random, RandomGen (split),
                      getStdGen, setStdGen, randomRIO, randomRs)
import Text.Printf (printf)

type Job = Int

randomRsIO :: Random a => (a, a) -> IO [a]
randomRsIO range = do
  g <- getStdGen
  let (g', g'') = split g
  setStdGen g''
  return $ randomRs range g'

genProblem :: Int -> Int -> IO [Job]
genProblem sz range = do
  sz' <- randomRIO (sz `div` 2, sz)
  fmap (take sz') $ randomRsIO (1, range)

algo1 :: [Job] -> Int
algo1 = go 0 0
  where go p1 p2 [] = max p1 p2
        go p1 p2 (j:js) | p1 <= p2 = go (p1 + j) p2 js
                        | otherwise = go p1 (p2 + j) js

algo2 :: [Job] -> Int
algo2 = algo1 . sortBy (\x y -> compare y x)

ss :: [a] -> [[a]]
ss [] = [[]]
ss (x:xs) = map (x:) (ss xs) ++ ss xs

opt :: [Job] -> Int
opt js = minimum (map (k . sum) (filter (not . null) subsets))
  where subsets = ss js
        total = sum js
        k s = max s (total - s)

main :: IO ()
main = do
  ps <- replicateM 1000 (genProblem 10 100)
  let (as1, as2) = unzip $ map test ps
  let (m1, m1problem) = maximumBy (compare `on` fst) $ zip as1 ps
  let (m2, m2problem) = maximumBy (compare `on` fst) $ zip as2 ps

  printf "Max ratio for algo1: %f\nProblem: %s\n" m1 (show m1problem)
  printf "Max ratio for algo2: %f\nProblem: %s\n" m2 (show m2problem)

  where test :: [Job] -> (Float, Float)
        test p = (a1 / optimal, a2 / optimal)
          where optimal = fromIntegral $ opt p
                a1 = fromIntegral $ algo1 p
                a2 = fromIntegral $ algo2 p
