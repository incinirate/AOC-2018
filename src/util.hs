{-# LANGUAGE LambdaCase #-}

module Util where

import Text.Parsec
import System.Environment

import qualified Data.Map as M

newtype Day n = Day n

readInput :: Show a => Day a -> IO String
readInput (Day day) = readFile =<< fileName
  where fileName :: IO String
        fileName = getArgs >>= (\case
          [_, fn] -> return fn
          _       -> return ("Day" ++ show day ++ ".txt"))


type Parser = Parsec String ()

parseStr :: String -> Parser a -> Either ParseError a
parseStr xs p = parse p "" xs

type TNumber = Int
numberParser :: Parser TNumber
numberParser = read <$> many digit

nonNumberParser :: Parser String
nonNumberParser = many1 (noneOf "0123456789")

extractOnlyNumListParser :: Parser [TNumber]
extractOnlyNumListParser = optional (skipMany1 nonNumberParser) *> (numberParser `sepBy` nonNumberParser)

reservedParser :: String -> Parser String
reservedParser name = do {walk name; return name}
  where walk :: String -> Parser ()
        walk = foldr ((*>) . char) (return ())


charMap :: [(Char, Char)] -> Char -> Char
charMap lSet c = ret $ cm M.!? c
  where cm = foldr (\ (k, v) acc -> M.insert k v acc) M.empty lSet
        ret (Just v) = v
        ret Nothing = c

charDown :: Char -> Char
charDown = charMap (zip ['A'..'Z'] ['a'..'z'])
