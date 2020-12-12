module Days.Day08
  ( runDay
  , runDayPartA
  , runDayPartB
  ) where

import Computer.Computer
  ( Instruction(ACC, JMP, NOP)
  , Program
  , ProgramState(accumulator, instructionPointer, instructionsRun)
  , getArgument
  , isJmp
  , isNop
  , runProgramUntil
  )
import Control.Applicative (Alternative((<|>)))
import Data.Attoparsec.Text
  ( Parser
  , decimal
  , endOfLine
  , sepBy
  , signed
  , space
  , string
  )
import Data.Text (Text)
import qualified Data.Vector as Vec
import qualified Program.RunDay as R (runDay, runDayPart)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

runDayPartA :: String -> IO OutputA
runDayPartA = R.runDayPart inputParser partA

runDayPartB :: String -> IO OutputB
runDayPartB = R.runDayPart inputParser partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (instruction `sepBy` endOfLine) >>= pure . Vec.fromList
  where
    instruction :: Parser Instruction
    instruction = nop <|> acc <|> jmp
    inst :: Text -> (Int -> Instruction) -> Parser Instruction
    inst s con = do
      arg <- string s *> space *> argument
      pure $ con arg
    nop :: Parser Instruction
    nop = inst "nop" NOP
    acc :: Parser Instruction
    acc = inst "acc" ACC
    jmp :: Parser Instruction
    jmp = inst "jmp" JMP
    argument :: Parser Int
    argument = signed decimal

------------ TYPES ------------
type Input = Program

type OutputA = Int

type OutputB = Int

instructionRunSecondTime :: ProgramState -> ProgramState -> p -> Bool
instructionRunSecondTime currentProgramState nextProgramState _ =
  instructionsRun currentProgramState == instructionsRun nextProgramState

------------ PART A ------------
partA :: Input -> OutputA
partA program = accumulator finalProgramState
  where
    finalProgramState :: ProgramState
    finalProgramState = runProgramUntil instructionRunSecondTime program

------------ PART B ------------
partB :: Input -> OutputB
partB program = accumulator terminatingProgram
  where
    terminatingProgram :: ProgramState
    terminatingProgram =
      head $
      dropWhile programDidntTerminate $
      map (runProgramUntil instructionRunSecondTime) programsToAttempt
    programDidntTerminate :: ProgramState -> Bool
    programDidntTerminate prog = instructionPointer prog < Vec.length program
    programsToAttempt :: [Vec.Vector Instruction]
    programsToAttempt = programsNopFlipped ++ programsJmpFlipped
    programsNopFlipped :: [Vec.Vector Instruction]
    programsNopFlipped =
      [ flipNop ix (getArgument (program Vec.! ix))
      | ix <- [0 .. Vec.length program - 1]
      , isNop $ program Vec.! ix
      ]
    programsJmpFlipped :: [Vec.Vector Instruction]
    programsJmpFlipped =
      [ flipJmp ix (getArgument (program Vec.! ix))
      | ix <- [0 .. Vec.length program - 1]
      , isJmp $ program Vec.! ix
      ]
    flipJmp :: Int -> Int -> Vec.Vector Instruction
    flipJmp i n = program Vec.// [(i, NOP n)]
    flipNop :: Int -> Int -> Vec.Vector Instruction
    flipNop i n = program Vec.// [(i, JMP n)]
