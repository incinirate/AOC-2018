module Day6 (run) where

import Data.List.Extra (group, sortOn)
import Util

data Point = Point { getX :: Int
                   , getY :: Int } 
                   deriving (Eq, Ord)

instance Show Point where
  show p = "[" ++ show (getX p) ++ "," ++ show (getY p) ++ "]"

parsePoint :: Parser Point
parsePoint = (\ [x, y] -> Point { getX = x, getY = y }) <$> extractOnlyNumListParser

getPoints :: String -> [Point]
getPoints xs = fromRight . (`parseStr` parsePoint) <$> lines xs


type Box = (Point, Point)
boundingBox :: [Point] -> Box
boundingBox ps = ( Point { getX = minimum xs, getY = minimum ys }
                 , Point { getX = maximum xs, getY = maximum ys } )
                 where xs = getX <$> ps
                       ys = getY <$> ps

mDist :: Point -> Point -> Int
mDist a b = abs (getX b - getX a) + abs (getY b - getY a)

closestPoint :: Point -> [Point] -> Maybe Point
closestPoint p ps = validate $ sortOn snd ((\x -> (x, mDist p x)) <$> ps)
  where validate ((pa, da):(_, db):_) = if da == db then Nothing else Just pa

inBorder :: Int -> Int -> Int -> Int -> Maybe Point -> Bool
inBorder _ _ _ _ Nothing = False
inBorder left top right bottom (Just Point { getX = x, getY = y })
  = (left /= x && right /= x) && (top /= y && bottom /= y)

run :: IO ()
run = do
  input <- readInput $ Day 6
  let points = getPoints input

  let (Point { getX = left , getY = top    },
       Point { getX = right, getY = bottom }) = boundingBox points

  let area = [Point { getX = x, getY = y } | x <- [left..right], y <- [top..bottom]]
  let eval = filter (inBorder left top right bottom) ((`closestPoint` points) <$> area)
  let largest = group (sortOn id eval)

  putStrLn $ "Part 1: " ++ show (maximum (length <$> largest))

  let dists = filter (<10000) $ (\x -> sum (mDist x <$> points)) <$> area
  putStrLn $ "Part 2: " ++ show (length dists)
