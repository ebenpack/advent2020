module Days.Day11
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
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
  seatLayoutMap <- coordinateParser charToSeat 0
  pure seatLayoutMap

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

type SeatLayoutMap = Map.Map Point Seat

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
  Map.mapWithKey
    (\coords seat ->
       simulationStep (findAdjacentSeats seatLayoutMap coords) seat) $
  seatLayoutMap

runSimulationToCompletion ::
     ([Seat] -> Seat -> Seat)
  -> (SeatLayoutMap -> Point -> [Seat])
  -> SeatLayoutMap
  -> SeatLayoutMap
runSimulationToCompletion simulationStep findAdjacentSeats = go
  where
    go seatLayoutMap =
      let nextSeatLayoutMap =
            runSimulationStep simulationStep findAdjacentSeats seatLayoutMap
       in if seatLayoutMap == nextSeatLayoutMap
            then seatLayoutMap
            else go nextSeatLayoutMap

partA :: Input -> OutputA
partA seatLayoutMap =
  Map.size $
  Map.filter seatIsOccupied $
  runSimulationToCompletion simulationStep findAdjacentSeats seatLayoutMap
  where
    findAdjacentSeats :: SeatLayoutMap -> Point -> [Seat]
    findAdjacentSeats seatLayoutMap (xCoord, yCoord) =
      [ seatLayoutMap Map.! (xCoord + x, yCoord + y)
      | x <- [-1 .. 1]
      , y <- [-1 .. 1]
      , x /= 0 || y /= 0
      , isJust $ seatLayoutMap Map.!? (xCoord + x, yCoord + y)
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
partB seatLayoutMap =
  Map.size $
  Map.filter seatIsOccupied $
  runSimulationToCompletion simulationStep visibleSeats seatLayoutMap
  where
    rayTrace :: SeatLayoutMap -> Point -> Step -> Seat
    rayTrace seatLayoutMap point step =
      case seatLayoutMap Map.!? step point of
        Nothing -> Floor
        Just Floor -> rayTrace seatLayoutMap (step point) step
        Just seat -> seat
    visibleSeats :: SeatLayoutMap -> Point -> [Seat]
    visibleSeats seatLayoutMap p =
      let steps =
            [ \(a, b) -> (a + x, b + y)
            | x <- [-1 .. 1]
            , y <- [-1 .. 1]
            , x /= 0 || y /= 0
            ]
       in [rayTrace seatLayoutMap p s | s <- steps]
    simulationStep :: [Seat] -> Seat -> Seat
    simulationStep adjacentSeats seat
      | seatIsEmpty seat && not (any seatIsOccupied adjacentSeats) =
        OccupiedSeat
      | seatIsOccupied seat && length (filter seatIsOccupied adjacentSeats) >= 5 =
        EmptySeat
      | otherwise = seat
