{-# LANGUAGE RecordWildCards, NamedFieldPuns, ApplicativeDo #-}

module Day12 (run) where

import Text.Parsec
import Text.Parsec.Char

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (foldl')

import Util

data Rule = Rule { input :: [Bool]
                 , output :: Bool  }

mapState :: Char -> Bool
mapState '.' = False
mapState '#' = True

parseState :: Parser [Bool]
parseState = do
  reservedParser "initial state: "
  state <- many (oneOf ".#")
  return (mapState <$> state)

parseRule :: Parser Rule
parseRule = do
  input <- count 5 (oneOf ".#")
  reservedParser " => "
  output <- oneOf ".#"
  return Rule { input = mapState <$> input, output = mapState output }

ruleToKey :: [Bool] -> Int
ruleToKey = foldl' (\ acc x -> fromEnum x + acc * 2) 0

runStep :: IntMap Bool -> IntMap Bool -> IntMap Bool
runStep ruleMap stateMap = M.fromList (zip range (willGrow <$> range))
  where getSurrounding k = fromMaybeBool <$> ((`M.lookup` stateMap) <$> [k-2..2+k])
        willGrow k = fromMaybeBool $ M.lookup (ruleToKey (getSurrounding k)) ruleMap
        range = [minimum (M.keys stateMap) - 2 .. maximum (M.keys stateMap) + 2]

run :: IO ()
run = do
  input <- readInput $ Day 12
  let (stateLine : _ : ruleLines) = lines input
  let state = fromRight $ parseStr parseState stateLine
  let rules = fromRight . parseStr parseRule <$> ruleLines
  let ruleMap = foldr (\ Rule{..} acc -> M.insert (ruleToKey input) output acc) M.empty rules
  let stateMap = M.fromList (zip [0..] state)

  let postGrow = foldr (\ x acc -> runStep ruleMap acc) stateMap [1..20]

  putStrLn $ "Part 1: " ++ show (sum (fst <$> filter snd (M.toList postGrow)))

  let postGrow2 = scanl (\ acc x -> runStep ruleMap acc) stateMap [1..200]
  let (r1:r2:reverseValues) = reverse ((\x -> sum (fst <$> filter snd (M.toList x))) <$> postGrow2)
  let finalValue = r1 + (50000000000 - 200)*(r1 - r2)

  putStrLn $ "Part 2: " ++ show finalValue
