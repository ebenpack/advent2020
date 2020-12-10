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
import qualified Data.Vector as Vec
import qualified Program.RunDay as R (runDay, runDayPart)

import Debug.Trace

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
    instruction = nop <|> acc <|> jmp
    inst s con = do
      arg <- string s *> space *> argument
      pure $ con arg
    nop = inst "nop" NOP
    acc = inst "acc" ACC
    jmp = inst "jmp" JMP
    argument = signed decimal

------------ TYPES ------------
type Input = Program

type OutputA = Int

type OutputB = Int

instructionRunSecondTime currentProgramState nextProgramState _ =
  instructionsRun currentProgramState == instructionsRun nextProgramState

------------ PART A ------------
partA :: Input -> OutputA
partA program = accumulator finalProgramState
  where
    finalProgramState = runProgramUntil instructionRunSecondTime program

------------ PART B ------------
partB :: Input -> OutputB
partB program = accumulator terminatingProgram
  where
    terminatingProgram =
      head $
      dropWhile programDidntTerminate $
      map (runProgramUntil instructionRunSecondTime) programsToAttempt
    programDidntTerminate prog = instructionPointer prog < Vec.length program
    programsToAttempt = programsNopFlipped ++ programsJmpFlipped
    programsNopFlipped =
      [ flipNop ix (getArgument (program Vec.! ix))
      | ix <- [0 .. Vec.length program - 1]
      , isNop $ program Vec.! ix
      ]
    programsJmpFlipped =
      [ flipJmp ix (getArgument (program Vec.! ix))
      | ix <- [0 .. Vec.length program - 1]
      , isJmp $ program Vec.! ix
      ]
    flipJmp i n = program Vec.// [(i, NOP n)]
    flipNop i n = program Vec.// [(i, JMP n)]
