module Days.Day04
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text
  ( Parser
  , char
  , decimal
  , digit
  , endOfInput
  , endOfLine
  , isHorizontalSpace
  , letter
  , many'
  , many1
  , parseOnly
  , satisfy
  , sepBy
  , skip
  , string
  )
import Data.Char (isHexDigit)
import Data.Either (isRight)
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Text (pack)
import Prelude hiding (take)
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` many1 endOfLine
  where
    passport :: Parser Passport
    passport = do
      items <- passportItem `sepBy` (skip isHorizontalSpace <|> endOfLine)
      pure $ foldl' addItemToPassport emptyPassport items
    passportItem :: Parser (String, String)
    passportItem = do
      name <- many1 letter <* ":"
      value <- many1 (letter <|> digit <|> char '#')
      pure (name, value)
    addItemToPassport :: Passport -> (String, String) -> Passport
    addItemToPassport p (name, value) =
      case name of
        "byr" -> p {byr = Just value}
        "iyr" -> p {iyr = Just value}
        "eyr" -> p {eyr = Just value}
        "hgt" -> p {hgt = Just value}
        "hcl" -> p {hcl = Just value}
        "ecl" -> p {ecl = Just value}
        "pid" -> p {pid = Just value}
        "cid" -> p {cid = Just value}
        _ -> p

------------ TYPES ------------
data Passport =
  Passport
    { byr :: Maybe String
    , iyr :: Maybe String
    , eyr :: Maybe String
    , hgt :: Maybe String
    , hcl :: Maybe String
    , ecl :: Maybe String
    , pid :: Maybe String
    , cid :: Maybe String
    }
  deriving (Show)

emptyPassport :: Passport
emptyPassport =
  Passport
    { byr = Nothing
    , iyr = Nothing
    , eyr = Nothing
    , hgt = Nothing
    , hcl = Nothing
    , ecl = Nothing
    , pid = Nothing
    , cid = Nothing
    }

type Input = [Passport]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length . passportsWithAllRequiredFields
  where
    passportsWithAllRequiredFields :: [Passport] -> [Passport]
    passportsWithAllRequiredFields = filter passPortHasAllRequiredFields
    passPortHasAllRequiredFields :: Passport -> Bool
    passPortHasAllRequiredFields passport =
      all (isJust . ($ passport)) requiredFields
    requiredFields :: [Passport -> Maybe String]
    requiredFields = [byr, iyr, eyr, hgt, hcl, ecl, pid]

------------ PART B ------------
between :: Int -> Int -> Parser ()
between low high = do
  d <- decimal <* endOfInput
  if d >= low && d <= high
    then pure ()
    else fail "not between"

eyeColor :: Parser ()
eyeColor = do
  col <- many' letter <* endOfInput
  case col of
    "amb" -> pure ()
    "blu" -> pure ()
    "brn" -> pure ()
    "gry" -> pure ()
    "grn" -> pure ()
    "hzl" -> pure ()
    "oth" -> pure ()
    _ -> fail "bad eyecolor"

height :: Parser ()
height = do
  d <- decimal
  unit <- (string "cm" <|> string "in") <* endOfInput
  case unit of
    "cm" ->
      if d >= 150 && d <= 193
        then pure ()
        else fail "bad cm height"
    "in" ->
      if d >= 59 && d <= 76
        then pure ()
        else fail "bad in height"
    _ -> fail "bad height"

hairColor :: Parser ()
hairColor = do
  hex <- char '#' *> many1 (satisfy isHexDigit) <* endOfInput
  if length hex == 6
    then pure ()
    else fail "bad hair color"

passportId :: Parser ()
passportId = do
  passId <- many' digit <* endOfInput
  if length passId == 9
    then pure ()
    else fail "bad pid"

partB :: Input -> OutputB
partB = length . passportsWithAllValidFields
  where
    passportsWithAllValidFields :: [Passport] -> [Passport]
    passportsWithAllValidFields = filter passportHasAllValidFields
    passportHasAllValidFields :: Passport -> Bool
    passportHasAllValidFields passport =
      all
        (\(validator, accessor) ->
           maybe False (validate validator) (accessor passport))
        fieldValidators
    validate :: Parser () -> String -> Bool
    validate validator s = isRight $ parseOnly validator (pack s)
    fieldValidators :: [(Parser (), Passport -> Maybe String)]
    fieldValidators =
      [ (between 1920 2002, byr)
      , (between 2010 2020, iyr)
      , (between 2020 2030, eyr)
      , (height, hgt)
      , (hairColor, hcl)
      , (eyeColor, ecl)
      , (passportId, pid)
      ]
