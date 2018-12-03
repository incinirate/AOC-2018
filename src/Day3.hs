module Day3 (run) where

import qualified Data.Array.MArray as MArray
import Data.Array.ST (runSTArray)
import Data.Array (Array, (!))

import Data.List.Extra (nubOrd, find)
import Control.Monad (forM_)

import Util

type Claim = (Int, Int, Int, Int, Int)

getNums :: String -> [String]
getNums "" = []
getNums xs
  = nums : getNums (deletePred notNums)
  where isNum = (`elem` "0123456789")
        deletePred = dropWhile (not . isNum)
        (nums, notNums) = span isNum (deletePred xs)

parse :: String -> Claim
parse xs = (a, b, a + c - 1, b + d - 1, id)
  where [id, a,b,c,d] = read <$> getNums xs :: [Int]

claimAreas :: Claim -> [(Int, Int)]
claimAreas (x1, y1, x2, y2, _) = [(x, y) | x <- [x1..x2], y <- [y1..y2]]

fill :: [Claim] -> Array (Int, Int) Int
fill claims = runSTArray $ do
  arr <- MArray.newArray ((0, 0), (1000, 1000)) 0

  let areas = concat (claimAreas <$> claims)
  nVals <- forM_ areas 
    (\index -> do
      nVal <- MArray.readArray arr index
      MArray.writeArray arr index (nVal + 1))

  return arr

ownArea :: Array (Int, Int) Int -> Claim -> Bool
ownArea arr claim = and ((== 1) . (arr !) <$> claimAreas claim)

run :: IO ()
run = do
  input <- readInput $ Day 3
  let claims = parse <$> lines input

  let arr = fill claims
  let areas = nubOrd (concat (claimAreas <$> claims))
  let cnt = length . filter (> 1) $ (arr!) <$> areas
  putStrLn $ "Part 1: " ++ show cnt ++ "in^2"

  let Just (_, _, _, _, ownID) = find (ownArea arr) claims
  putStrLn $ "Part 2: ID " ++ show ownID
