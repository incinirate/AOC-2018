module Day2 (run) where

import Data.List (nub)
import Data.Maybe
import Util

data Score = Score { has2Letters :: Bool
                   , has3Letters :: Bool }

score :: Eq a => [a] -> Score
score xs =
  Score {has2Letters = 2 `elem` letterCounts, has3Letters = 3 `elem` letterCounts}
  where letters = nub xs
        letterCounts = length . flip filter xs . (==) <$> letters
        has2Letters = 2 `elem` letterCounts
        has3Letters = 3 `elem` letterCounts

diff :: Eq a => [a] -> [a] -> [a]
diff xs ys =
  fromJust <$> filter (/= Nothing) (zipWith test xs ys)
  where test a b = if a == b then Just a else Nothing

run :: IO ()
run = do
  input <- readInput $ Day 2
  let ids = lines input
      scores = score <$> ids
      twos = length (filter has2Letters scores)
      threes = length (filter has3Letters scores)

  putStrLn $ "Checksum: " ++ show (twos * threes)

  let commonChars = head [diff xs ys | xs <- ids, ys <- ids, length (diff xs ys) + 1 == length xs]
  putStrLn $ "Differing Letters: " ++ commonChars
