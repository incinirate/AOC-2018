{-# LANGUAGE ApplicativeDo #-}

module Day8 (run) where

import Text.Parsec

import Util

type Children = [Tree]
type Meta = [Int]
data Tree = Node Children Meta
            deriving Show

nodeParser :: Parser Tree
nodeParser = do
  childCnt <- numberParser;              spaces
  metaCnt  <- numberParser;              spaces
  children <- count childCnt nodeParser; spaces
  meta <- count metaCnt (numberParser <* spaces)
  return (Node children meta)

sumMeta :: Tree -> Int
sumMeta (Node kids meta) = sum meta + sum (sumMeta <$> kids)

index :: [a] -> Int -> Maybe a
index xs i = if i < 0 || i >= length xs then Nothing else Just (xs !! i)

nodeValue :: Maybe Tree -> Int
nodeValue Nothing = 0
nodeValue (Just (Node kids meta))
  | kidCnt == 0 = sum meta
  | otherwise = sum (nodeValue . index kids <$> (pred <$> meta))
  where kidCnt = length kids

run :: IO ()
run = do
  input <- readInput $ Day 8
  let root = fromRight $ parseStr nodeParser input
  
  putStrLn $ "Part 1: " ++ show (sumMeta root)
  putStrLn $ "Part 2: " ++ show (nodeValue (Just root))
