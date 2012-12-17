{-# LANGUAGE NamedFieldPuns #-}
import Control.Arrow ( (&&&) )
import Data.List ( sortBy, foldl' )

type Weight = Int
type Length = Int
type Score  = Int

data Job = Job { weight :: Weight
               , len    :: Length
               }
         deriving Show

score :: Job -> Score
score (Job {weight, len}) = weight - len

cmpScoredJobs :: (Job, Score) -> (Job, Score) -> Ordering
cmpScoredJobs (j1, s1) (j2, s2)
  | s1 == s2  = compare (weight j2) (weight j1)
  | otherwise = compare s2 s1 -- note the reverse order of scores

readInput :: IO [Job]
readInput = fmap (map parseLine . tail . lines) (readFile "jobs.txt")
  where parseLine s = let [w, l] = words s
                      in Job { weight = read w, len = read l }

schedule :: [Job] -> [Job]
schedule jobs = map fst $ sortBy cmpScoredJobs scored
  where scored = map (id &&& score) jobs

weightedSum :: [Job] -> Int
weightedSum = fst . foldl' k (0, 0)
  where k (s, c) (Job {weight, len}) = (s + weight * c', c')
          where c' = len + c

main :: IO ()
main = do
  input <- readInput
  print $ weightedSum $ schedule input
