module Days.Day14
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
  , many1
  , sepBy
  , string
  )
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
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
inputParser = (mask <|> mem) `sepBy` endOfLine
  where
    mem :: Parser BitMask
    mem = do
      addr <- string "mem[" *> decimal <* string "] = "
      val <- decimal
      pure $ Mem addr val
    mask :: Parser BitMask
    mask = (string "mask = " *> many1 bitmask) >>= (pure . Mask . Vec.fromList)
    bitmask :: Parser Bit
    bitmask = x <|> i <|> o
    x :: Parser Bit
    x = char 'X' >> pure X
    i :: Parser Bit
    i = char '1' >> pure I
    o :: Parser Bit
    o = char '0' >> pure O

------------ TYPES ------------
data Bit
  = X
  | O
  | I
  deriving (Show, Eq)

data BitMask
  = Mask (Vector Bit)
  | Mem Integer Integer
  deriving (Show)

type Input = [BitMask]

type OutputA = Integer

type OutputB = Integer

------------ HELPERS ------------
emptyField :: Vector Integer
emptyField = Vec.fromList $ replicate 36 0

valueToBitField :: Integer -> Vector Integer
valueToBitField = go emptyField 35
  where
    go acc ix v
      | v == 0 || ix < 0 = acc
      | v `rem` 2 == 1 = go (acc Vec.// [(ix, 1)]) (ix - 1) (v `quot` 2)
      | otherwise = go acc (ix - 1) (v `quot` 2)

bitFieldToValue :: Vector Integer -> Integer
bitFieldToValue v = go 0 35
  where
    go acc ix
      | ix < 0 = acc
      | v Vec.! ix == 1 = go (acc + (2 ^ (35 - ix))) (ix - 1)
      | otherwise = go acc (ix - 1)

setWithMask :: BitMask -> Vector Integer -> Integer
setWithMask (Mask mask) val = go 0 35
  where
    getVal :: Int -> Integer
    getVal ix
      | val Vec.! ix == 0 = 0
      | otherwise = 2 ^ (35 - ix)
    go :: Integer -> Int -> Integer
    go acc ix
      | ix < 0 = acc
      | mask Vec.! ix == X = go (acc + getVal ix) (ix - 1)
      | mask Vec.! ix == I = go (acc + (2 ^ (35 - ix))) (ix - 1)
      | mask Vec.! ix == O = go acc (ix - 1)
      | otherwise = go acc (ix - 1)

------------ PART A ------------
partA :: Input -> OutputA
partA xs =
  Map.foldl (+) 0 $
  doStuff (Mask $ Vec.fromList $ replicate 36 X) (Map.empty) xs
  where
    doStuff ::
         BitMask -> Map Integer Integer -> [BitMask] -> Map Integer Integer
    doStuff _ mem [] = mem
    doStuff _ mem (m@(Mask _):xs) = doStuff m mem xs
    doStuff mask mem (Mem addr val:xs) =
      doStuff
        mask
        (Map.insert addr (setWithMask mask (valueToBitField val)) mem)
        xs

------------ PART B ------------
setAddrsWithFloatingMask ::
     Vector Integer
  -> Map Integer Integer
  -> BitMask
  -> Integer
  -> Map Integer Integer
setAddrsWithFloatingMask addr mem (Mask mask) val =
  foldl' (\a b -> Map.insert (bitFieldToValue b) val a) mem $ getAddrs addr 35
  where
    getAddrs :: Vector Integer -> Int -> [Vector Integer]
    getAddrs addr ix
      | ix < 0 = [addr]
      | mask Vec.! ix == O = getAddrs addr (ix - 1)
      | mask Vec.! ix == I = getAddrs (addr Vec.// [(ix, 1)]) (ix - 1)
      | mask Vec.! ix == X =
        getAddrs (addr Vec.// [(ix, 1)]) (ix - 1) ++
        getAddrs (addr Vec.// [(ix, 0)]) (ix - 1)

partB :: Input -> OutputB
partB xs =
  Map.foldl (+) 0 $ doStuff (Mask $ Vec.fromList $ replicate 36 X) Map.empty xs
  where
    doStuff ::
         BitMask -> Map Integer Integer -> [BitMask] -> Map Integer Integer
    doStuff _ mem [] = mem
    doStuff _ mem (m@(Mask _):xs) = doStuff m mem xs
    doStuff mask mem (Mem addr val:xs) =
      doStuff
        mask
        (setAddrsWithFloatingMask (valueToBitField addr) mem mask val)
        xs
