{-# LANGUAGE LambdaCase #-}

module Util where

import Text.Parsec
import qualified Text.Parsec.Number as NumParsers
import System.Environment

import qualified Data.Map as M

newtype Day n = Day n

readInput :: Show a => Day a -> IO String
readInput (Day day) = readFile =<< fileName
  where fileName :: IO String
        fileName = getArgs >>= (\case
          [_, fn] -> return fn
          _       -> return ("Day" ++ show day ++ ".txt"))

fromRight :: Either a b -> b
fromRight (Right x) = x

fromMaybeBool :: Maybe Bool -> Bool
fromMaybeBool (Just a) = a
fromMaybeBool Nothing = False

type Parser = Parsec String ()

parseStr :: Parser a -> String -> Either ParseError a
parseStr p = parse p ""

type TNumber = Int
numberParser :: Parser TNumber
numberParser = NumParsers.int

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


applyOn :: (a -> b) -> (b -> b -> c) -> a -> a -> c
applyOn fstFn sndFn a b = sndFn (fstFn a) (fstFn b)

eqOn :: Eq b => (a -> b) -> a -> a -> Bool
eqOn fn = applyOn fn (==)

compareOn :: Ord b => (a -> b) -> a -> a -> Ordering
compareOn fn = applyOn fn compare


pullToPair :: (a -> b) -> a -> (b, a)
pullToPair fn x = (fn x, x)
