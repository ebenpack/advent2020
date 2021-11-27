module Days.Day19 where

import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text
  ( Parser
  , char
  , decimal
  , endOfLine
  , letter
  , many1
  , sepBy
  , string
  )
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
  rs <- rule `sepBy` endOfLine
  endOfLine
  endOfLine
  s <- str `sepBy` endOfLine
  pure (rs, s)
  where
    rule :: Parser (Int, Rule)
    rule = do
      d <- decimal <* string ": "
      r <- letterRule <|> orRule <|> subRule
      pure (d, r)
    letterRule = do
      l <- char '"' *> letter <* char '"'
      pure $ CharRule l
    orRule = do
      r1 <- subRule <* string " | "
      r2 <- subRule
      pure $ OrRule r1 r2
    subRule = (decimal `sepBy` char ' ') >>= (pure . Subrule)
    str = many1 letter

------------ TYPES ------------
data Rule
  = CharRule Char
  | Subrule [Int]
  | OrRule Rule Rule
  deriving (Show)

type Input = ([(Int, Rule)], [String])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
ruleListToMap :: [(Int, Rule)] -> Map Int Rule
ruleListToMap rs = foldl' (\m (num, rule) -> Map.insert num rule m) Map.empty rs

compile :: Map Int Rule -> Rule -> String -> (String -> Bool) -> Bool
compile _ (CharRule c) "" _ = False
compile _ (CharRule c) (x:xs) k =
  if x == c
    then k xs
    else False
compile rules (Subrule (r:rs)) xs k =
  compile rules (rules Map.! r) xs (flip (compile rules (Subrule rs)) k)
compile _ (Subrule []) xs k = k xs
compile rules (OrRule r1 r2) xs k =
  compile rules r1 xs k || compile rules r2 xs k

partA :: Input -> OutputA
partA (rules, strs) =
  let ruleMap = ruleListToMap rules
   in length $ filter (\s -> compile ruleMap (ruleMap Map.! 0) s null) strs

------------ PART B ------------
partB :: Input -> OutputB
partB (rules, strs) =
  let ruleMap =
        Map.union
          (Map.fromList
             [ (8, OrRule (Subrule [42]) (Subrule [42, 8]))
             , (11, OrRule (Subrule [42, 31]) (Subrule [42, 11, 31]))
             ])
          (ruleListToMap rules)
   in length $ filter (\s -> compile ruleMap (ruleMap Map.! 0) s null) strs
