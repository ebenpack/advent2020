module Days.Day01
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy)
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser [Int]
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- TODO: What's the abstraction here?
partA :: Input -> OutputA
partA xs =
  head $
  [ x * y
  | (x, xn) <- zip xs [1 ..]
  , (y, yn) <- zip xs [1 ..]
  , xn < yn
  , x + y == 2020
  ]

------------ PART B ------------
partB :: Input -> OutputB
partB xs =
  head $
  [ x * y * z
  | (x, xn) <- zip xs [1 ..]
  , (y, yn) <- zip xs [1 ..]
  , (z, zn) <- zip xs [1 ..]
  , xn < yn && yn < zn
  , x + y + z == 2020
  ]
