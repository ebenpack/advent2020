module Days.Day18
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Combinators (between)
import Data.Attoparsec.Combinator (sepBy)
import Data.Attoparsec.Text
  ( Parser
  , char
  , decimal
  , endOfLine
  , manyTill
  , sepBy
  , space
  )
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = expression `sepBy` endOfLine
  where
    expression = (bracketed <|> add <|> multiply <|> val) `sepBy` char ' '
    val = do
      d <- decimal
      pure $ Val d
    op c cons = do
      char c
      pure cons
    add = op '+' Add
    multiply = op '*' Multiply
    bracketed = do
      char '('
      e <- expression
      char ')'
      pure $ Bracketed e

------------ TYPES ------------
data Expression
  = Val Int
  | Add
  | Multiply
  | Bracketed [Expression]
  deriving (Show, Eq)

type Input = [[Expression]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . map evaluate
  where
    evaluate [] = 0
    evaluate [Val n] = n
    evaluate (Bracketed b:xs) = evaluate (Val (evaluate b) : xs)
    evaluate (Val m:Add:Bracketed b:xs) = evaluate (Val (m + evaluate b) : xs)
    evaluate (Val m:Multiply:Bracketed b:xs) =
      evaluate (Val (m * evaluate b) : xs)
    evaluate (Val m:Add:Val n:xs) = evaluate (Val (m + n) : xs)
    evaluate (Val m:Multiply:Val n:xs) = evaluate (Val (m * n) : xs)

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . map evaluate
  where
    evaluateAdd :: [Expression] -> [Expression]
    evaluate = evaluateMul . evaluateAdd
    evaluateAdd [] = []
    evaluateAdd [Val n] = [Val n]
    evaluateAdd (Bracketed b:xs) = evaluateAdd (Val (evaluate b) : xs)
    evaluateAdd (Val m:Add:Bracketed b:xs) =
      evaluateAdd (Val (m + evaluate b) : xs)
    evaluateAdd (Val m:Add:Val n:xs) = evaluateAdd (Val (m + n) : xs)
    evaluateAdd (x:xs) = x : evaluateAdd xs
    evaluateMul :: [Expression] -> Int
    evaluateMul [] = 0
    evaluateMul [Val n] = n
    evaluateMul (Bracketed b:xs) = evaluateMul (Val (evaluate b) : xs)
    evaluateMul (Val m:Multiply:Bracketed b:xs) =
      evaluateMul (Val (m * evaluate b) : xs)
    evaluateMul (Val m:Multiply:Val n:xs) = evaluateMul (Val (m * n) : xs)
