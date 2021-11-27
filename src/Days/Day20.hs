module Days.Day20
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
import qualified Util.Util as U

import Data.Attoparsec.Text
import Data.Void
import Debug.Trace
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 tile
  where
    tile = do
      t <- tileNumber <* endOfLine
      b <- body
      pure (t, b)
    tileNumber = string "Tile " *> decimal <* string ":"
    body = coordinateParser pixel 1
    pixel '.' = Just Empty
    pixel '#' = Just Occupied
    pixel _ = Nothing

------------ TYPES ------------
data Pixel
  = Empty
  | Occupied
  deriving (Show, Eq, Ord)

type Tile = Map (Int, Int) Pixel

type Input = [(Int, Tile)]

type Edge = [Pixel]

type OutputA = Int

type OutputB = Void

------------ HELPERS ------------
corrnerTiles :: Input -> [Int]
corrnerTiles xs = filter isCornerTile tiles
  where
    isCornerTile :: Int -> Bool
    isCornerTile tile =
      let edges = edgeMap Map.! tile
          sharedEdges = filter (\e -> (length $ pixelMap Map.! e) > 1) edges
       in length sharedEdges == 2
    tileSize :: Int
    tileSize = round $ sqrt $ fromIntegral (Map.size $ snd $ head xs)
    tiles :: [Int]
    tiles = Map.keys edgeMap
    pixelMap :: Map Edge (Set Int)
    pixelMap =
      Map.foldlWithKey'
        (\m k ps -> foldl' (\m p -> Map.alter (insert k) p m) m ps)
        Map.empty
        edgeMap
      where
        insert :: Int -> Maybe (Set Int) -> Maybe (Set Int)
        insert x Nothing = Just $ Set.singleton x
        insert x (Just s) = Just $ Set.insert x s
    edgeMap :: Map Int [Edge]
    edgeMap = Map.fromList $ map (\(c, t) -> (c, getEdges t)) xs
    canonicalizeEdge :: Edge -> Edge
    canonicalizeEdge ps = min ps (reverse ps)
    getEdges :: Tile -> [Edge]
    getEdges m =
      let a = Map.assocs m
          getEdge :: ((Int, Int) -> Bool) -> Edge
          getEdge f = map snd $ filter (f . fst) a
          e1 = getEdge (\(x, y) -> x == 1)
          e2 = getEdge (\(x, y) -> y == 1)
          e3 = getEdge (\(x, y) -> x == tileSize)
          e4 = getEdge (\(x, y) -> y == tileSize)
       in canonicalizeEdge <$> [e1, e2, e3, e4]

------------ PART A ------------
partA :: Input -> OutputA
partA xs = product $ corrnerTiles xs

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"