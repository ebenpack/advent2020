module Computer.Computer where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Instruction
  = ACC Int
  | JMP Int
  | NOP Int
  deriving (Show)

getArgument :: Instruction -> Int
getArgument (ACC n) = n
getArgument (JMP n) = n
getArgument (NOP n) = n

isNop :: Instruction -> Bool
isNop (NOP _) = True
isNop _ = False

isJmp :: Instruction -> Bool
isJmp (JMP _) = True
isJmp _ = False

isAcc :: Instruction -> Bool
isAcc (ACC _) = True
isAcc _ = False

type Program = Vector Instruction

data ProgramState =
  ProgramState
    { accumulator :: Int
    , instructionPointer :: Int
    , instructionsRun :: Set Int
    }
  deriving (Show)

executeInstruction :: Int -> Instruction -> ProgramState -> ProgramState
executeInstruction instPtr (ACC n) p =
  p
    { accumulator = accumulator p + n
    , instructionPointer = instructionPointer p + 1
    , instructionsRun = Set.insert instPtr (instructionsRun p)
    }
executeInstruction instPtr (JMP n) p =
  p
    { instructionPointer = instructionPointer p + n
    , instructionsRun = Set.insert instPtr (instructionsRun p)
    }
executeInstruction instPtr (NOP _) p =
  p
    { instructionPointer = instructionPointer p + 1
    , instructionsRun = Set.insert instPtr (instructionsRun p)
    }

runProgramUntil ::
     (ProgramState -> ProgramState -> Instruction -> Bool)
  -> Program
  -> ProgramState
runProgramUntil pred program =
  go
    ProgramState
      {accumulator = 0, instructionPointer = 0, instructionsRun = Set.empty}
  where
    go currentProgramState =
      let instructionPntr = instructionPointer currentProgramState
          possibleInstruction = program Vec.!? instructionPntr
          -- An instruction pointer beyond the program boundaries indicates
          -- that the program was run to completion
       in case possibleInstruction of
            Nothing -> currentProgramState
            Just instruction ->
              let nextProgramState =
                    executeInstruction
                      instructionPntr
                      instruction
                      currentProgramState
               in if pred currentProgramState nextProgramState instruction
                    then currentProgramState
                    else go nextProgramState
