{-# LANGUAGE ApplicativeDo #-}

module Day4 (run) where

import Data.List (sort, maximumBy)
import Text.Parsec
import qualified Data.Map.Strict as M

import Util

type Date = (Int, Int, Int)
type Time = (Int, Int)
type DateAndTime = (Date, Time)

data Action = FellAsleep | WokeUp | SwitchShift Int
              deriving (Show, Eq)

data Event = Event DateAndTime Action
             deriving (Show, Eq)

instance Ord Event where
  (Event date1 _) <= (Event date2 _) = date1 <= date2

-- Parser Combinators for extracting data
innerTimeSeps :: Parser String
innerTimeSeps = many1 (oneOf "-: ")

numListParser :: Parser [Int]
numListParser = optional (skipMany1 innerTimeSeps) *> (numberParser `sepBy` innerTimeSeps)

listToDateTime :: [Int] -> (Date, Time)
listToDateTime [year, month, date, hour, minute] = ((year, month, date), (hour, minute))

dateParser :: Parser DateAndTime
dateParser = listToDateTime <$> between (char '[') (char ']') numListParser

actionParser :: Parser Event
actionParser = Event <$> dateParser <*> (spaces *>
      ((FellAsleep  <$  try (reservedParser "falls asleep"))
   <|> (WokeUp      <$  try (reservedParser "wakes up"))
   <|>  SwitchShift <$> try (reservedParser "Guard #" *> numberParser)))

-- Action collection and manipulation functions
incr :: (Num v, Ord k) => k -> M.Map k v -> M.Map k v
incr k m = M.insert k (1 + M.findWithDefault 0 k m) m

type GState k v = (M.Map k v, Int, Int)
genState :: (GState k v -> M.Map k v) -> GState k v -> Event -> GState k v
genState insertFn (map, guard, lastFall) (Event (_, (_, min)) event) = case event of
  (SwitchShift newID) -> (map, newID, -1)
  FellAsleep -> (map, guard, min)
  WokeUp -> (foldl (\acc i -> insertFn (acc, guard, i)) map [lastFall..(min - 1)], guard, -1) 

runState :: (GState k v -> Event -> GState k v) -> [Event] -> M.Map k v
runState stateFn xs = (\(m, _, _) -> m) $ foldl stateFn (M.empty, 0, -1) xs

countAssociative :: [Event] -> M.Map Int (M.Map Int Int)
countAssociative = runState $ genState (\(m, g, i) -> M.insert g (incr i (M.findWithDefault M.empty g m)) m)

countFlat :: [Event] -> M.Map (Int, Int) Int
countFlat = runState $ genState (\(m, g, i) -> incr (g, i) m)

maxVal :: Ord v => M.Map k v -> (k, v)
maxVal m = maximumBy (\ (_, a) (_, b) -> a `compare` b) (M.toList m)

run :: IO ()
run = do
  input <- readInput $ Day 4
  let events = sort ((\(Right v) -> v) . flip parseStr actionParser <$> lines input)

  let totals = countAssociative events
  let afterSleep = countFlat events

  let (guard, _) = maxVal (sum <$> totals)
  putStrLn $ "Part 1: " ++ show (guard * (fst . maxVal $ totals M.! guard))

  let ((guard, min), _) = maxVal afterSleep
  putStrLn $ "Part 2: " ++ show (guard * min)
