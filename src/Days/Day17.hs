module Days.Day17
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Program.RunDay as R (runDay, runDayPart)
import Util.Parsers (coordinateParser)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  m <- coordinateParser cube 0
  pure $
    Map.foldlWithKey' (\s (x, y) _ -> Set.insert (x, y, 0, 0) s) Set.empty m
  where
    cube :: Char -> Maybe Bool
    cube '#' = Just True
    cube _ = Nothing

------------ TYPES ------------
data Cube
  = Active
  | Inactive
  deriving (Show, Eq)

type Point = (Int, Int, Int, Int)

type CubeMap = Set Point

type Input = CubeMap

type OutputA = Int

type OutputB = Int

------------ HELPERS ------------
addPoints :: Point -> Point -> Point
addPoints (x1, y1, z1, w1) (x2, y2, z2, w2) =
  (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

runCycle :: (Point -> [Point]) -> CubeMap -> CubeMap
runCycle possibleNeighbors grid = Set.foldl' changeState grid pointsToConsider
  where
    neighboringPoints :: Set Point
    neighboringPoints =
      Set.fromList $ do
        p <- Set.toList grid -- TODO
        possibleNeighbors p
    pointsToConsider :: CubeMap
    pointsToConsider = Set.union grid neighboringPoints
    changeState :: CubeMap -> Point -> CubeMap
    changeState newMap point
      | pointIsActive point &&
          (length (activeNeighbors point) == 2 ||
           length (activeNeighbors point) == 3) = newMap
      | pointIsActive point = Set.delete point newMap
      | not (pointIsActive point) && length (activeNeighbors point) == 3 =
        Set.insert point newMap
      | otherwise = newMap
    pointIsActive :: Point -> Bool
    pointIsActive point = Set.member point grid
    activeNeighbors :: Point -> CubeMap
    activeNeighbors p =
      Set.filter (\p -> Set.member p grid) $ Set.fromList $ possibleNeighbors p

runCycles :: (CubeMap -> CubeMap) -> Int -> CubeMap -> CubeMap
runCycles runCycle 0 acc = acc
runCycles runCycle n acc = runCycles runCycle (n - 1) (runCycle acc)

------------ PART A ------------
partA :: Input -> OutputA
partA xs = Set.size $ runCycles run3DCycle 6 xs
  where
    possible3DNeighbors :: Point -> [Point]
    possible3DNeighbors p =
      [ addPoints p (x, y, z, 0)
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , z <- [-1 .. 1]
      , (x, y, z) /= (0, 0, 0)
      ]
    run3DCycle :: CubeMap -> CubeMap
    run3DCycle = runCycle possible3DNeighbors

------------ PART B ------------
partB :: Input -> OutputB
partB xs = Set.size $ runCycles run4DCycle 6 xs
  where
    possible4DNeighbors :: Point -> [Point]
    possible4DNeighbors p =
      [ addPoints p (x, y, z, w)
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , z <- [-1 .. 1]
      , w <- [-1 .. 1]
      , (x, y, z, w) /= (0, 0, 0, 0)
      ]
    run4DCycle :: CubeMap -> CubeMap
    run4DCycle = runCycle possible4DNeighbors
