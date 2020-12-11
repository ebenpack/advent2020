module Days.Day11
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Util.Parsers (coordinateParser)

import Data.Attoparsec.Text
import Data.Void
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  seatLayoutMap <- coordinateParser charToSeat 0
  let maxX = maximum $ map fst $ Map.keys seatLayoutMap
  let maxY = maximum $ map snd $ Map.keys seatLayoutMap
  pure $ SeatLayoutMap {seatMap = seatLayoutMap, width = maxX, height = maxY}

charToSeat :: Char -> Maybe Seat
charToSeat '.' = Just Floor
charToSeat 'L' = Just EmptySeat
charToSeat 'X' = Just OccupiedSeat
charToSeat _ = Nothing

------------ TYPES ------------
type Point = (Int, Int)

type Step = Point -> Point

data Seat
  = Floor
  | EmptySeat
  | OccupiedSeat
  deriving (Show, Eq)

seatIsEmpty :: Seat -> Bool
seatIsEmpty EmptySeat = True
seatIsEmpty _ = False

seatIsOccupied :: Seat -> Bool
seatIsOccupied OccupiedSeat = True
seatIsOccupied _ = False

data SeatLayoutMap =
  SeatLayoutMap
    { seatMap :: Map.Map Point Seat
    , width :: Int
    , height :: Int
    }
  deriving (Show, Eq)

type Input = SeatLayoutMap

type OutputA = Int

type OutputB = Int

------------ PART A ------------
runSimulationStep ::
     ([Seat] -> Seat -> Seat)
  -> (SeatLayoutMap -> Point -> [Seat])
  -> SeatLayoutMap
  -> SeatLayoutMap
runSimulationStep simulationStep findAdjacentSeats seatLayoutMap =
  seatLayoutMap
    { seatMap =
        Map.mapWithKey
          (\coords seat ->
             simulationStep (findAdjacentSeats seatLayoutMap coords) seat) $
        seatMap seatLayoutMap
    }

partA :: Input -> OutputA
partA seatLayoutMap = go seatLayoutMap
  where
    go seatLayoutMap =
      let nextSeatLayoutMap =
            runSimulationStep simulationStep findAdjacentSeats seatLayoutMap
       in if seatLayoutMap == nextSeatLayoutMap
            then Map.size $ Map.filter seatIsOccupied $ seatMap seatLayoutMap
            else go nextSeatLayoutMap
    findAdjacentSeats :: SeatLayoutMap -> Point -> [Seat]
    findAdjacentSeats seatLayoutMap (xCoord, yCoord) =
      [ seatMap seatLayoutMap Map.! (xCoord + x, yCoord + y)
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , x /= 0 || y /= 0
      , isJust $ seatMap seatLayoutMap Map.!? (xCoord + x, yCoord + y)
      ]
    simulationStep :: [Seat] -> Seat -> Seat
    simulationStep adjacentSeats seat
      | seatIsEmpty seat && not (any seatIsOccupied adjacentSeats) =
        OccupiedSeat
      | seatIsOccupied seat && length (filter seatIsOccupied adjacentSeats) >= 4 =
        EmptySeat
      | otherwise = seat

------------ PART B ------------
partB :: Input -> OutputA
partB seatLayoutMap = go seatLayoutMap
  where
    go seatLayoutMap =
      let nextSeatLayoutMap =
            runSimulationStep simulationStep visibleSeats seatLayoutMap
       in if seatLayoutMap == nextSeatLayoutMap
            then Map.size $ Map.filter seatIsOccupied $ seatMap seatLayoutMap
            else go nextSeatLayoutMap
    traceRay :: SeatLayoutMap -> Point -> Step -> Seat
    traceRay seatLayoutMap point step =
      case seatMap seatLayoutMap Map.!? step point of
        Nothing -> Floor
        Just Floor -> traceRay seatLayoutMap (step point) step
        Just seat -> seat
    visibleSeats :: SeatLayoutMap -> Point -> [Seat]
    visibleSeats seatLayoutMap p =
      let steps =
            [ \(a, b) -> (a + x, b + y)
            | x <- [-1 .. 1]
            , y <- [-1 .. 1]
            , x /= 0 || y /= 0
            ]
       in [traceRay seatLayoutMap p s | s <- steps]
    simulationStep :: [Seat] -> Seat -> Seat
    simulationStep adjacentSeats seat
      | seatIsEmpty seat && not (any seatIsOccupied adjacentSeats) =
        OccupiedSeat
      | seatIsOccupied seat && length (filter seatIsOccupied adjacentSeats) >= 5 =
        EmptySeat
      | otherwise = seat
