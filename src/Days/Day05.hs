module Days.Day05
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser, char, choice, endOfLine, sepBy)
import Data.List (foldl')
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
inputParser = boardingPass `sepBy` endOfLine
  where
    boardingPass =
      BoardingPass <$> fb <*> fb <*> fb <*> fb <*> fb <*> fb <*> fb <*> lr <*>
      lr <*>
      lr
    fb = choice [char 'F' >> pure F, char 'B' >> pure B]
    lr = choice [char 'L' >> pure L, char 'R' >> pure R]

------------ TYPES ------------
data FB
  = F
  | B
  deriving (Show, Bounded, Eq)

data LR
  = L
  | R
  deriving (Show, Bounded, Eq)

data BoardingPass =
  BoardingPass FB FB FB FB FB FB FB LR LR LR
  deriving (Show)

type Input = [BoardingPass]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
boardingPassToSeatCoords :: BoardingPass -> (Int, Int)
boardingPassToSeatCoords (BoardingPass fb1 fb2 fb3 fb4 fb5 fb6 fb7 lr1 lr2 lr3) =
  (findRow [fb1, fb2, fb3, fb4, fb5, fb6, fb7], findCol [lr1, lr2, lr3])
  where
    binaryPartition :: (Show a, Bounded a, Eq a) => [a] -> (Int, Int) -> Int
    binaryPartition [] boundaries = fst boundaries
    binaryPartition [x] boundaries =
      if x == minBound
        then fst boundaries
        else snd boundaries
    binaryPartition (x:xs) boundaries =
      binaryPartition xs $ partition boundaries x
    partition :: (Show a, Bounded a, Eq a) => (Int, Int) -> a -> (Int, Int)
    partition (start, end) x =
      let half = (end - start) `div` 2
       in if x == minBound
            then (start, end - (half + 1))
            else (start + half + 1, end)
    findCol :: [LR] -> Int
    findCol xs = binaryPartition xs (0, 7)
    findRow :: [FB] -> Int
    findRow xs = binaryPartition xs (0, 127)

coordsToSeatId :: (Int, Int) -> Int
coordsToSeatId (row, col) = (row * 8) + col

boardingPassToSeatId :: BoardingPass -> Int
boardingPassToSeatId = coordsToSeatId . boardingPassToSeatCoords

partA :: Input -> OutputA
partA = maximum . (boardingPassToSeatId <$>)

------------ PART B ------------
partB :: Input -> OutputB
partB boardingpasses = head unoccupiedseats
  where
    unoccupiedseats =
      [ coordsToSeatId (row, col)
      | row <- [0 .. 127]
      , col <- [0 .. 7]
      , let seatId = coordsToSeatId (row, col)
         in not (Set.member seatId occupiedSeats) &&
            all (`Set.member` occupiedSeats) [seatId + 1, seatId - 1]
      ]
    occupiedSeats =
      foldl'
        (\s x -> Set.insert (boardingPassToSeatId x) s)
        Set.empty
        boardingpasses
