module Day9 (run) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List.Ordered
import Data.List (foldl')
import Debug.Trace
import Util

increaseScore :: Int -> Int -> IntMap Int -> IntMap Int
increaseScore player pts = M.alter incr player 
  where incr Nothing = Just pts
        incr (Just x) = Just (x + pts)

type State = (Int, IntMap Int, [Int], Int)
doStep :: Int -> State -> Int -> State
doStep playerCnt (cMarble, scoreMap, circle, cPlayer) x
  | x `mod` 23 == 0 = ( sevenCClock `mod` (length circle - 1)
                      , increaseScore cPlayer (traceShowId x + y) scoreMap
                      , as ++ bs
                      , nextPlayer )
  | otherwise = (twoClock, scoreMap, xs ++ [traceShowId x] ++ ys, nextPlayer)
    where (xs, ys) = splitAt twoClock circle
          (as, y:bs) = splitAt sevenCClock circle
          twoClock = 1 + ((cMarble + 1) `mod` length circle)
          sevenCClock = (cMarble - 7) `mod` length circle
          nextPlayer = (cPlayer `mod` playerCnt) + 1

doAll :: Int -> [Int] -> State -> State
doAll playerCnt marbles state
  = foldl' (doStep playerCnt) state marbles


run :: IO ()
run = do
  input <- readInput $ Day 9

  let playerCnt:lastWorth:_ = fromRight $ parseStr extractOnlyNumListParser input
  let circle = [0, 1]
  let marbles = [2..lastWorth]
  let (_, map, _, _) = doAll playerCnt marbles (1, M.empty, circle, 2)
  putStrLn $ "Part 1: " ++ show (maximum (M.elems map))

  let marbles2 = [2..lastWorth*100]
  let (_, map2, _, _) = doAll playerCnt marbles2 (1, M.empty, circle, 2)
  putStrLn $ "Part 2: " ++ show (maximum (M.elems map2))
  