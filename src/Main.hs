module Main where

import System.Environment (getArgs)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

runDay :: [String] -> IO ()
runDay xs
  | (day : _) <- xs
  = case day of
    "1" -> Day1.run
    "2" -> Day2.run
    "3" -> Day3.run
    "4" -> Day4.run
    ___ -> fail $ "Invalid day '" ++ day ++ "'"

  | otherwise = fail "Syntax: ./aoc <day> [input]"

main :: IO ()
main = do
  args <- getArgs
  runDay args