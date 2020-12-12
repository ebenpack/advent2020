module Days.Day09
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import Data.List (tails)
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
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

partA :: Input -> OutputA
partA xmas =
  head $
  head -- double head... very safe, lol
    [ x
    | x <- scanl (\b a -> a : take 25 b) [] xmas
    , length x >= 26
    , not $ any (\(a, b) -> a + b == head x) (pairs $ drop 1 x)
    ]

------------ PART B ------------
partB :: Input -> OutputB
partB xmas = go xmas
  where
    target :: OutputA
    target = partA xmas
    go :: Input -> OutputA
    go ls@(_:xs) = do
      case contiguousSequence target ls [] 0 of
        Nothing -> go xs
        Just cs ->
          if length cs >= 2
            then minimum cs + maximum cs
            else go xs
    contiguousSequence :: Int -> Input -> [OutputB] -> Int -> Maybe [OutputB]
    contiguousSequence target [] acc n =
      if n == target
        then Just acc
        else Nothing
    contiguousSequence target (x:xs) acc n
      | n == target = Just acc
      | n > target = Nothing
      | otherwise = contiguousSequence target xs (x : acc) (n + x)
