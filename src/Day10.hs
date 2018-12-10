{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Day10 (run) where

import Text.Parsec
import Text.Parsec.Char
import Data.List (intercalate)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Util

data Point = Point {  x :: Int,  y :: Int
                   , vx :: Int, vy :: Int }
                   deriving Show

parsePoint :: Parser Point
parsePoint = do
  reservedParser "position=<"; spaces
  x <- numberParser; skipMany (oneOf ", ")
  y <- numberParser; reservedParser "> velocity=<"; spaces
  vx <- numberParser; skipMany (oneOf ", ")
  vy <- numberParser
  return Point { x, y, vx, vy }

timeStep :: Point -> Point
timeStep Point{..} = Point { x = x + vx, y = y + vy, vx, vy }

range :: [Point] -> Int
range ps = maximum ys - minimum ys + 1
  where ys = y <$> ps

findMessage :: (Int, [Point]) -> (Int, [Point])
findMessage (time, ps)
  | range ps == 10 = (time, ps)
  | otherwise = findMessage (time + 1, timeStep <$> ps)

printPoints :: [Point] -> String
printPoints ps = intercalate "\n" (getLine <$> range)
  where (xs, ys) = (x <$> ps, y <$> ps)
        range = [minimum ys .. maximum ys]
        domain = [minimum xs .. maximum xs]
        emptyLine = foldr (\x acc -> M.insert x ' ' acc) M.empty domain
        getPts yc = filter ((==yc) . y) ps
        getLine yc = snd <$> M.toList (foldr (\x acc -> M.insert x 'X' acc) emptyLine (x <$> getPts yc))

run :: IO ()
run = do
  input <- readInput $ Day 10

  let points = fromRight <$> (parseStr parsePoint <$> lines input)
  let (totalTime, message) = findMessage (0, points)
  putStrLn $ "Part 1:\n" ++ printPoints message
  putStrLn $ "Part 2: " ++ show totalTime
