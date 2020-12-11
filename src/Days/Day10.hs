module Days.Day10
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Monad.State (MonadState(get), evalState, forM, modify)
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Integer]

type OutputA = Integer

type OutputB = Integer

------------ PART A ------------
partA :: Input -> OutputA
partA xs = countNJoltDiffs 1 0 input * countNJoltDiffs 3 0 input
  where
    input = 0 : sort xs ++ [maximum xs + 3]
    countNJoltDiffs _ acc [] = acc
    countNJoltDiffs _ acc [_] = acc
    countNJoltDiffs n acc (x:y:xs) =
      if y - x == n
        then countNJoltDiffs n (acc + 1) (y : xs)
        else countNJoltDiffs n acc (y : xs)

------------ PART B ------------
partB :: Input -> OutputB
partB xs = answer
  where
    input = 0 : sort xs ++ [maximum xs + 3]
    answer = evalState (go 0 input) Map.empty
    go _ [] = pure 0
    go n (x:xs)
      | x - n > 3 = return 0
      | null xs = return 1
      | otherwise = do
        memo <- get
        case Map.lookup x memo of
          Just v -> pure v
          Nothing -> do
            let nextSteps = [drop m xs | m <- [0 .. 3]]
            result <- sum <$> forM nextSteps (go x)
            modify $ Map.insert x result
            pure result
