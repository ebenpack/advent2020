module Days.Day07
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text
  ( Parser
  , char
  , decimal
  , endOfLine
  , letter
  , many1
  , manyTill
  , sepBy
  , space
  , string
  )
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
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
inputParser = do
  bags <- bagDef `sepBy` endOfLine
  pure $ foldl' joinBags Map.empty (concat bags)
  where
    joinBags :: Map String Bag -> Bag -> Map String Bag
    joinBags m b = Map.alter (mergeBags b) (color b) m
    mergeBags :: Bag -> Maybe Bag -> Maybe Bag
    mergeBags b1 mb =
      case mb of
        Nothing -> Just b1
        Just b2 ->
          Just $
          b2
            { containedBy = containedBy b2 ++ containedBy b1
            , contains = contains b2 ++ contains b1
            }
    bagDef :: Parser [Bag]
    bagDef = do
      t <- bagType <* string " contain "
      bags <- (empty <|> contain) <* char '.'
      let childBags =
            map
              (\(c, n) -> Bag {color = c, contains = [], containedBy = [(t, n)]})
              bags
      pure $ Bag {color = t, containedBy = [], contains = bags} : childBags
    bagType :: Parser String
    bagType = do
      t <- manyTill (word <* space) (string "bags" <|> string "bag")
      pure $ unwords t
    word :: Parser String
    word = many1 letter
    empty :: Parser [(String, Int)]
    empty = string "no other bags" >> pure []
    numBags :: Parser (String, Int)
    numBags = do
      n <- decimal <* space
      b <- bagType
      pure (b, n)
    contain :: Parser [(String, Int)]
    contain = numBags `sepBy` string ", "

------------ TYPES ------------
data Bag =
  Bag
    { color :: String
    , containedBy :: [(String, Int)]
    , contains :: [(String, Int)]
    }
  deriving (Show)

type Input = Map String Bag

type OutputA = Int

type OutputB = Int

------------ PART A ------------
solveThingusA :: String -> Input -> Set String
solveThingusA target bagMap = go Set.empty target
  where
    go :: Set String -> String -> Set String
    go visited current =
      let nextBag = Map.lookup current bagMap
          toVisit = maybe [] ((fst <$>) . containedBy) nextBag
       in if Set.member current visited
            then visited
            else foldl'
                   (\a b -> Set.union (Set.insert b a) (go visited b))
                   visited
                   toVisit

partA :: Input -> OutputA
partA = Set.size . solveThingusA "shiny gold"

------------ PART B ------------
solveThingusB :: String -> Input -> Int
solveThingusB target bagMap = go target
  where
    go :: String -> Int
    go current =
      let nextBag = Map.lookup current bagMap
       in case nextBag of
            Nothing -> 0
            Just b -> sum $ map (\(c, m) -> m + (m * go c)) (contains b)

partB :: Input -> OutputB
partB = solveThingusB "shiny gold"
