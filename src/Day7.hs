{-# LANGUAGE ApplicativeDo, TupleSections #-}

module Day7 (run) where

import Text.Parsec.Char (letter)
import Data.List.Extra (sortOn, groupBy, nubOrd)

import           Data.Set (Set)
import qualified Data.Set as S

import           Data.Map (Map)
import qualified Data.Map as M

import Util

data Dependency = Dep { required :: Char
                      , result   :: Char }
                      deriving (Show, Eq)

parseRequirement :: Parser Dependency
parseRequirement = (\ (d, r) -> Dep { required = d, result = r }) <$> do
  reservedParser "Step ";  requirement <- letter
  reservedParser " must be finished before step "; result <- letter
  return (requirement, result)

nextStep :: Set Char -> [(Char, [Dependency])] -> Char
nextStep done deps = fst . head $ filter (all ((`S.member` done) . required) . snd) deps

evalTree :: [(Char, [Dependency])] -> String
evalTree = evalStep S.empty
  where evalStep _ [] = ""
        evalStep done deps = step : evalStep (step `S.insert` done) (filter ((/=step) . fst) deps)
          where step = nextStep done deps

makeDepMap :: [Dependency] -> [(Char, [Dependency])]
makeDepMap deps = sortOn fst $ (pullToPair (result . head) <$> grouped) ++ ((,[]) <$> S.toList extra)
  where grouped = groupBy (eqOn result) deps
        requirements = foldr S.insert S.empty (required <$> deps)
        extra = foldr S.delete requirements (result <$> deps)

run :: IO ()
run = do
  input <- readInput $ Day 7
  let deps = sortOn result $ fromRight . parseStr parseRequirement <$> lines input
  let depMap = makeDepMap deps
  putStrLn $ "Part 1: " ++ show (evalTree depMap)
-- TODO: Part 2
