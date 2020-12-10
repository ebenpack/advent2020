module Days.Day03
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Map.Strict as Map
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
  sledMap <- coordinateParser charToSquare 0
  let maxX = maximum $ map fst $ Map.keys sledMap
  pure $ SledMap {unMap = \(x, y) -> Map.lookup (x `mod` (maxX + 1), y) sledMap}

charToSquare :: Char -> Maybe Square
charToSquare '#' = Just Tree
charToSquare '.' = Just OpenGround
charToSquare _ = Nothing

------------ TYPES ------------
type Point = (Int, Int)

type Step = (Int -> Int, Int -> Int)

data Square
  = Tree
  | OpenGround
  deriving (Show)

newtype SledMap =
  SledMap
    { unMap :: Point -> Maybe Square
    }

instance Show SledMap where
  show _ = "<SledMap>"

type Input = SledMap

type OutputA = Int

type OutputB = Int

------------ HELPERS ------------
findTreesInSlope :: Input -> Step -> OutputA
findTreesInSlope sledMap (xstep, ystep) = go 0 (0, 0)
  where
    go n pos@(x, y) =
      let nextPos = (xstep x, ystep y)
       in case unMap sledMap pos of
            Nothing -> n
            Just Tree -> go (n + 1) nextPos
            Just OpenGround -> go n nextPos

------------ PART A ------------
partA :: Input -> OutputA
partA sledMap = findTreesInSlope sledMap ((+ 3), (+ 1))

------------ PART B ------------
partB :: Input -> OutputB
partB sledMap =
  product $
  findTreesInSlope sledMap <$>
  [ ((+ 1), (+ 1))
  , ((+ 3), (+ 1))
  , ((+ 5), (+ 1))
  , ((+ 7), (+ 1))
  , ((+ 1), (+ 2))
  ]
