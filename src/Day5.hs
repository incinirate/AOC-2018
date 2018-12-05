module Day5 (run) where

import qualified Data.Map as M
import Data.List (foldl')

import Util

charDown :: Char -> Char
charDown c = ret $ cm M.!? c
  where cm = foldr (\ (k, v) acc -> M.insert k v acc) M.empty lSet
        lSet = zip "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "abcdefghijklmnopqrstuvwxyz"
        ret (Just v) = v
        ret Nothing = c

react :: String -> String
react xs = foldl' (\acc x -> 
                  if safeHead acc /= x 
                  && (charDown . safeHead) acc == charDown x 
                    then tail acc
                    else x:acc) [head xs] (tail xs)
  where safeHead xs
          | null xs = ' '
          | otherwise = head xs

part2 :: String -> Int
part2 xs =
  minimum (length . react <$> stripped)
  where stripped = [filter (noneOfChar i) xs | i <- ['A'..'Z']]
        noneOfChar i x = (x /= i) && (x /= charDown i)

run :: IO ()
run = do
  input <- init <$> readInput (Day 5)  -- Init because of trailing \n
  
  putStrLn $ "Part 1: " ++ show (length $ react input)
  putStrLn $ "Part 2: " ++ show (part2 input)
