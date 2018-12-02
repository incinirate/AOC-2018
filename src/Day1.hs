import System.Environment
import qualified Data.Set as Set

import Util

stripPlus :: String -> String
stripPlus ('+':xs) = xs
stripPlus xs = xs

firstDup :: Ord a => [a] -> Maybe a
firstDup xs = findDup xs Set.empty
  where findDup [] _ = Nothing
        findDup (x:xs) ys = if Set.member x ys
                              then Just x
                              else findDup xs (Set.insert x ys)

main = do
  input <- readInput $ Day 1
  let nums = read . stripPlus <$> lines input :: [Int]

  putStrLn $ "Part 1: " ++ (show . sum $ nums)

  putStrLn $ "Part 2: " ++ (show . firstDup . scanl (+) 0 $ cycle nums)
