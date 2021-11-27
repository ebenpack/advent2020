module Days.Day15
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser, char, decimal, sepBy)
import Data.Map.Strict (Map)
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
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ HELPERS ------------
solve :: Int -> [Int] -> Int
solve target xs =
  go (last xs) (length xs + 1) $ Map.fromList (zip xs (map (\x -> [x]) [1 ..]))
  where
    alterTurn :: a -> Maybe [a] -> Maybe [a]
    alterTurn x Nothing = Just [x]
    alterTurn x (Just xs) = Just $ take 4 (x : xs)
    go :: Int -> Int -> Map Int [Int] -> Int
    go prev turn mem
      | turn == target + 1 = prev
      | Map.member prev mem && length (mem Map.! prev) == 1 =
        go 0 (turn + 1) (Map.alter (alterTurn turn) 0 mem)
      | Map.member prev mem && length (mem Map.! prev) > 1 =
        let val = (head (mem Map.! prev) - (mem Map.! prev) !! 1)
         in go val (turn + 1) (Map.alter (alterTurn turn) val mem)
      | otherwise = go 0 (turn + 1) (Map.alter (alterTurn turn) 0 mem)

------------ PART A ------------
partA :: Input -> OutputA
partA = solve 2020

------------ PART B ------------
partB :: Input -> OutputB
partB = solve 30000000
