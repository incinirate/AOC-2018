module Day11 (run) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Util

-- | Puzzle Input
serialNumber :: Int
serialNumber = 9445

coordToI :: (Int, Int) -> Int
coordToI (x, y) = (y - 1)*300 + x

powerGrid :: IntMap Int
powerGrid = foldr (\c acc -> M.insert (coordToI c) (getPower c) acc) M.empty [(x, y) | x <- [1..300], y <- [1..300]]

getPower :: (Int, Int) -> Int
getPower (x, y)
  = hundreds (((rackID * y) + serialNumber) * rackID) - 5
  where rackID = x + 10
        hundreds x = (x `quot` 100) `mod` 10

sumPower :: Int -> (Int, Int) -> (Int, (Int, Int))
sumPower size coord@(sx, sy) = (sum (fromMaybe <$> powerLevels), coord)
  where powerLevels = flip M.lookup powerGrid <$> [coordToI (x, y) | x <- [sx .. sx + size - 1], y <- [sy .. sy + size - 1]]
        fromMaybe (Just x) = x
        fromMaybe Nothing = 0

getMax :: Int -> (Int, (Int, Int))
getMax size = maximum (sumPower size <$> [(x, y) | x <- [1..(300 - size + 1)], y <- [1..(300 - size + 1)]])

run :: IO ()
run = do
  let maxP1 = getMax 3
  putStrLn $ "Part 1: " ++ show maxP1
  let maxP2 = maximum [(getMax s, s) | s <- [1..300]]
  putStrLn $ "Part 2: " ++ show maxP2
