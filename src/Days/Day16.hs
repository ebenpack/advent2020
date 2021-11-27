module Days.Day16
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
  , many1
  , sepBy
  , space
  , string
  )
import Data.List (find, groupBy, isInfixOf, sortBy)
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
  r <- rule `sepBy` endOfLine
  endOfLine
  endOfLine
  myTicket <-
    do string "your ticket:"
       endOfLine
       t <- ticket
       endOfLine
       endOfLine
       pure t
  nearbyTickets <-
    do string "nearby tickets:"
       endOfLine
       ticket `sepBy` endOfLine
  pure (r, myTicket, nearbyTickets)
  where
    ticket = decimal `sepBy` char ','
    rule = do
      n <- word `sepBy` space
      string ": "
      low <- range
      string " or "
      high <- range
      pure $ Rule {name = unwords n, lowRange = low, highRange = high}
    range = do
      n1 <- decimal <* char '-'
      n2 <- decimal
      pure (n1, n2)
    word = many1 letter

------------ TYPES ------------
type Range = (Int, Int)

data Rule =
  Rule
    { name :: String
    , lowRange :: Range
    , highRange :: Range
    }
  deriving (Show, Eq)

type Ticket = [Int]

type Input = ([Rule], Ticket, [Ticket])

type OutputA = Int

type OutputB = Int

------------ HELPERS ------------
between :: Int -> (Int, Int) -> Bool
between n (low, high) = n >= low && n <= high

removeValidFields :: [Rule] -> Ticket -> Ticket
removeValidFields rules = filter (not . isTicketFieldValid rules)

isTicketFieldValid :: [Rule] -> Int -> Bool
isTicketFieldValid rules n =
  any (between n . lowRange) rules || any (between n . highRange) rules

isTicketValid :: [Rule] -> Ticket -> Bool
isTicketValid rules = all (isTicketFieldValid rules)

isRuleSatisfied :: Rule -> Int -> Bool
isRuleSatisfied r n = between n (lowRange r) || between n (highRange r)

------------ PART A ------------
partA :: Input -> OutputA
partA (rules, _, nearbyTickets) = errorRate
  where
    errorRate = sum $ map (sum . removeValidFields rules) nearbyTickets

------------ PART B ------------
partB :: Input -> OutputB
partB (rules, myTicket, nearbyTickets) = solve rules []
  where
    ticketLength = length $ head nearbyTickets
    validTickets = filter (isTicketValid rules) nearbyTickets
    slice n = map (!! n)
    validFieldsForRule n r acc
      | n < 0 = acc
      | all (isRuleSatisfied r) (slice n validTickets) =
        validFieldsForRule (n - 1) r ((n, r) : acc)
      | otherwise = validFieldsForRule (n - 1) r acc
    findFinalSolution [] acc =
      let rules = filter (\(_, y) -> isInfixOf "departure" (name y)) acc
       in product (map (\(x, _) -> myTicket !! x) rules)
    findFinalSolution xs acc =
      let onie = find (\x -> length x == 1) xs
       in case onie of
            Nothing -> error "Whoopsie!"
            Just [x@(y, z)] ->
              let newWithXRemoved =
                    map (filter (\(a, b) -> a /= y && z /= b)) xs
                  newWithEmptyRemoved = filter (not . null) newWithXRemoved
               in findFinalSolution newWithEmptyRemoved (x : acc)
    solve :: [Rule] -> [(Int, Rule)] -> Int
    solve [] acc =
      let groupedPossibleFieldsForRule =
            groupBy (\(a, _) (b, _) -> a == b) $
            sortBy (\(a, _) (b, _) -> compare a b) acc
       in findFinalSolution groupedPossibleFieldsForRule []
    solve (r:rs) acc =
      let possibleFieldsForRule = validFieldsForRule (ticketLength - 1) r []
       in solve rs (possibleFieldsForRule ++ acc)
