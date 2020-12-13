module Days.Day13
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy)
import Data.List (minimumBy)
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
  est <- estimate <* endOfLine
  busIds <- busId `sepBy` char ','
  pure (est, busIds)
  where
    estimate :: Parser Int
    estimate = decimal
    busId :: Parser BusId
    busId = (char 'x' >> pure OutOfService) <|> (decimal >>= pure . Id)

------------ TYPES ------------
data BusId -- TODO: This name doesn't really work
  = Id Int
  | OutOfService
  deriving (Show, Eq)

type Input = (Int, [BusId])

type OutputA = Int

type OutputB = Integer

------------ HELPERS ------------
isOutOfService :: BusId -> Bool
isOutOfService OutOfService = True
isOutOfService _ = False

getBusId :: BusId -> Int
getBusId (Id busId) = busId -- TODO: Partial... YOLO!

------------ PART A ------------
partA :: Input -> OutputA
partA (estimate, busIds) = waitTime bestBus * bestBus
  where
    bussesInService :: [BusId]
    bussesInService = filter (not . isOutOfService) busIds
    waitTime :: Int -> Int
    waitTime busId = (((estimate `div` busId) + 1) * busId) - estimate -- TODO: This could probably be simplified
    bestBus :: Int
    bestBus =
      fst $
      minimumBy
        (\a b -> compare (snd a) (snd b))
        ([(x, waitTime x) | Id x <- bussesInService])

------------ PART B ------------
partB :: Input -> OutputB
partB (_, busIds) = busSieve busses 1 1
  where
    busses :: [(Integer, BusId)]
    busses = filter (not . isOutOfService . snd) $ zip [0 ..] busIds
    getMod :: Integer -> BusId -> Integer -> Int
    getMod t (Id busId) offset =
      let t' = fromIntegral t
       in ((t' `mod` busId) + (fromIntegral offset)) `mod` busId
    busSieve :: [(Integer, BusId)] -> Integer -> Integer -> Integer
    busSieve [] current _ = current
    busSieve (x@(offsetX, busX):xs) current add
      | getMod current busX offsetX == 0 =
        busSieve xs current (lcm add (fromIntegral $ getBusId busX))
      | otherwise = busSieve (x : xs) (current + add) add
