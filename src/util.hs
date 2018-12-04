{-# LANGUAGE LambdaCase #-}

module Util where

import Text.Parsec
import System.Environment

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
numberParser = read <$> many (oneOf "0123456789")

nonNumberParser :: Parser String
nonNumberParser = many1 (noneOf "0123456789")

extractNumListParser :: Parser [TNumber]
extractNumListParser = do
  optional $ skipMany1 nonNumberParser
  numberParser `sepBy` nonNumberParser
