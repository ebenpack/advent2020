module Days.Day06
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser, endOfLine, letter, many1, sepBy)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = group `sepBy` endOfLine
  where
    group = person `sepBy` endOfLine
    person = do
      answers <- many1 letter
      pure $ Set.fromList answers

------------ TYPES ------------
type PersonAnswers = Set Char

type Group = [PersonAnswers]

type Input = [Group]

type OutputA = Int

type OutputB = Int

------------ HELPERS ------------
collapseGroupSets :: [Set Char] -> Set Char
collapseGroupSets = foldl' (flip Set.union) Set.empty

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . groupAnswerCounts
  where
    groupAnswerCounts = map (Set.size . collapseGroupSets)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map (length . allAnswered)
  where
    allAnswered group =
      [ answer
      | answer <- Set.toList (collapseGroupSets group)
      , all (Set.member answer) group
      ]
