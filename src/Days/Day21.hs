module Days.Day21 (runDay, runDayPartA, runDayPartB) where

import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, runDayPart)
import Data.Attoparsec.Text
import Data.Void

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser ::Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
