module Days.Day12
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy, string)
import Data.List (foldl')
import Data.Text (Text)
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where
    instruction :: Parser Instruction
    instruction = n <|> s <|> e <|> w <|> l <|> r <|> f
    inst :: Text -> (Int -> Instruction) -> Parser Instruction
    inst s con = do
      u <- string s *> unit
      pure $ con u
    unit :: Parser Int
    unit = decimal
    n :: Parser Instruction
    n = inst "N" N
    s :: Parser Instruction
    s = inst "S" S
    e :: Parser Instruction
    e = inst "E" E
    w :: Parser Instruction
    w = inst "W" W
    l :: Parser Instruction
    l = inst "L" L
    r :: Parser Instruction
    r = inst "R" R
    f :: Parser Instruction
    f = inst "F" F

------------ TYPES ------------
type Point = (Int, Int)

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Enum, Bounded, Eq)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where
    add :: Integral a => a -> a -> a -> a
    add mod x y = (x + y + mod) `rem` mod

data Location =
  Location
    { dir :: Direction
    , point :: Point
    , waypoint :: Point
    }
  deriving (Show, Eq)

data Instruction
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving (Show, Eq)

type Input = [Instruction]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
startingLocation :: Location
startingLocation = Location {point = (0, 0), dir = East, waypoint = (10, 1)}

followDirection :: Location -> Instruction -> Location
followDirection = go
  where
    go :: Location -> Instruction -> Location
    go l@Location {point = (x, y)} (N n) = l {point = (x, y + n)}
    go l@Location {point = (x, y)} (S n) = l {point = (x, y - n)}
    go l@Location {point = (x, y)} (E n) = l {point = (x + n, y)}
    go l@Location {point = (x, y)} (W n) = l {point = (x - n, y)}
    go l i@(L _) = rotate i l
    go l i@(R _) = rotate i l
    go l@Location {dir = d} (F n) = go l (directionToInstruction d n)
    directionToInstruction :: Direction -> Int -> Instruction
    directionToInstruction East n = E n
    directionToInstruction West n = W n
    directionToInstruction North n = N n
    directionToInstruction South n = S n
    rotate :: Instruction -> Location -> Location
    rotate (L n) l@Location {dir = d}
      | n <= 0 = l
      | otherwise = rotate (L (n - 90)) (l {dir = prev d})
    rotate (R n) l@Location {dir = d}
      | n <= 0 = l
      | otherwise = rotate (R (n - 90)) (l {dir = next d})

partA :: Input -> OutputA
partA = absPlus . point . foldl' followDirection startingLocation
  where
    absPlus :: Num a => (a, a) -> a
    absPlus (x, y) = abs x + abs y

------------ PART B ------------
followWaypoint :: Location -> Instruction -> Location
followWaypoint = go
  where
    go :: Location -> Instruction -> Location
    go l@Location {waypoint = (x, y)} (N n) = l {waypoint = (x, y + n)}
    go l@Location {waypoint = (x, y)} (S n) = l {waypoint = (x, y - n)}
    go l@Location {waypoint = (x, y)} (E n) = l {waypoint = (x + n, y)}
    go l@Location {waypoint = (x, y)} (W n) = l {waypoint = (x - n, y)}
    go l (L 0) = l
    go l@Location {waypoint = (x, y)} (L n) =
      go (l {waypoint = (negate y, x)}) (L (n - 90))
    go l (R 0) = l
    go l@Location {waypoint = (x, y)} (R n) =
      go (l {waypoint = (y, negate x)}) (R (n - 90))
    go l@Location {point = (px, py), waypoint = (wx, wy)} (F n) =
      (l {point = (px + (wx * n), py + (wy * n))})

partB :: Input -> OutputB
partB = absPlus . point . foldl' followWaypoint startingLocation
  where
    absPlus :: Num a => (a, a) -> a
    absPlus (x, y) = abs x + abs y
