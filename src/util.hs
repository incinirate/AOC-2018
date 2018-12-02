{-# LANGUAGE LambdaCase #-}

module Util where

import System.Environment

newtype Day n = Day n

readInput :: Show a => Day a -> IO String
readInput (Day day) = readFile =<< fileName
  where fileName :: IO String
        fileName = getArgs >>= (\case
          (fn:_) -> return fn
          _      -> return ("Day" ++ show day ++ ".txt"))
