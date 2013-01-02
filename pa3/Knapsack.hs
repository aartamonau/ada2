import Data.Array ( Array, (!), listArray, array )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.MemoTrie ( memo2 )
import Control.Monad.State ( State, gets, modify, evalState )
import System.Environment ( getArgs )

type Value = Int
type Weight = Int
type Item = (Value, Weight)

type Items = Array Int Item

readInput :: FilePath -> IO (Weight, Int, Items)
readInput path = do
  content <- fmap lines $ readFile path
  let [maxWeight, n] = map read $ words (head content)
  let items = map parseItem $ take n (tail content)
  let itemsArray = listArray (1, n) items

  return (maxWeight, n, itemsArray)

  where parseItem :: String -> Item
        parseItem s = (read v, read w)
          where [v, w] = words s

knapsack :: (Weight, Int, Items) -> Value
knapsack (maxWeight, n, items) = k' n maxWeight
  where k _ 0 = 0
        k 1 w
          | weight 1 <= w = value 1
          | otherwise     = 0
        k i w
          | weight i <= w = max (k' (i - 1) w) (k' (i - 1) (w - weight i) + value i)
          | otherwise     = k' (i - 1) w

        k' = memo2 k

        weight i = snd $ items ! i
        value i  = fst $ items ! i

knapsack2 :: (Weight, Int, Items) -> Value
knapsack2 (maxWeight, n, items) = knot ! (n, maxWeight)
  where knot = array ((1, 0), (n, maxWeight)) ks

        ks = [ ((i, 0), 0) | i <- [1..n] ] ++
             [ ((1, w), n1 w) | w <- [1..maxWeight] ] ++
             [ ((i, w), k i w) | i <- [2..n], w <- [1..maxWeight] ]

        n1 w | weight 1 <= w = value 1
             | otherwise     = 0
        k i w
          | weight i <= w = max (knot ! (i - 1, w))
                                (knot ! (i - 1, w - weight i) + value i)
          | otherwise     = knot ! (i - 1, w)


        weight i = snd $ items ! i
        value i  = fst $ items ! i

knapsack3 :: (Weight, Int, Items) -> Value
knapsack3 (maxWeight, n, items) = evalState (kMemo n maxWeight) Map.empty
  where kMemo :: Int -> Weight -> State (Map (Int, Weight) Value) Value
        kMemo _ 0 = return 0
        kMemo 1 w
          | weight 1 <= w = return $ value 1
          | otherwise     = return 0
        kMemo i w = do
          cached <- getCached i w
          case cached of
            Just v ->
              return v
            Nothing -> do
              v <- if weight i <= w
                     then do
                       v1 <- kMemo (i - 1) w
                       v2 <- fmap (+value i) $ kMemo (i - 1) (w - weight i)

                       return $ max v1 v2
                     else
                       kMemo (i - 1) w

              updateCache i w v
              return v

        getCached i w = gets (Map.lookup (i, w))
        updateCache i w v = modify (Map.insert (i, w) v)

        weight i = snd $ items ! i
        value i  = fst $ items ! i

main :: IO ()
main = do
  args <- getArgs
  let path | [path'] <- args = path'
           | otherwise       = "knapsack1.txt"

  print =<< fmap knapsack3 (readInput path)
