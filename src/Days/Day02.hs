module Days.Day02
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Data.Attoparsec.Text
  ( Parser
  , char
  , decimal
  , endOfLine
  , letter
  , many'
  , sepBy
  , space
  )
import Data.Text (Text, index, pack, unpack)

import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parsePassword `sepBy` endOfLine
  where
    parsePassword = do
      firstDigit <- decimal <* char '-'
      secondDigit <- decimal <* space
      character <- letter <* char ':' <* many' space
      password <- many' letter
      pure $
        Password
          { firstDigit = firstDigit
          , secondDigit = secondDigit
          , character = character
          , password = pack password
          }

------------ TYPES ------------
data Password =
  Password
    { firstDigit :: Int
    , secondDigit :: Int
    , character :: Char
    , password :: Text
    }
  deriving (Show)

type Input = [Password]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter isValidLowHighPassword
  where
    isValidLowHighPassword :: Password -> Bool
    isValidLowHighPassword p =
      let count = length $ filter (== character p) $ unpack $ password p
       in count >= firstDigit p && count <= secondDigit p

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter isValidPositionMatchPassword
  where
    isValidPositionMatchPassword :: Password -> Bool
    isValidPositionMatchPassword p =
      let p1 = index (password p) $ firstDigit p - 1
          p2 = index (password p) $ secondDigit p - 1
          c = character p
       in length (filter id [p1 == c, p2 == c]) == 1
