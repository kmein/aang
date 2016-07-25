-- | The main module contains all the functionality of the "aang" program.
module Main where

import Control.Monad.State (evalState, state)
import Data.Char (toLower, toUpper)
import Math.Combinat.Permutations (permuteMultiset)
import System.Environment (getArgs)

-- | Get a word as command line argument and output
-- all possible element deconstructions of that word.
--
-- >>> ./aang Hask
-- H As K
main :: IO ()
main =
    do args <- fmap toLower <$> getArgs
       periodicTable <- (lines . fmap toLower) <$> readFile "periodic-table.txt"
       forM_ args $ \arg ->
           mapM_ (putStrLn . concatCapitalized) $ periods periodicTable arg
    where
      -- | >>> concatCapitalized ["ca","o","s"]
      -- "Ca O S"
      concatCapitalized :: [String] -> String
      concatCapitalized = unwords . map capitalize
          where
            capitalize [] = []
            capitalize (x:xs) = toUpper x : xs

isPeriodic :: String -> Bool
isPeriodic tbl = not . null . periods tbl . map toLower

-- | Find all string combos of which all substrings are contained within
-- the element symbol in the periodic table of the elements.
periods :: [String] -> String -> [[String]]
periods periodicTable = filter (all (`elem` periodicTable)) . stringCombos

-- | Split a string into all possible groups of 1 and 2.
--
-- >>> stringCombos "word"
-- [["w","o","r","d"],["wo","r","d"],["w","or","d"],["w","o","rd"],["wo","rd"]]
stringCombos :: String -> [[String]]
stringCombos str = map (`partitionString` str) $ combos $ length str

-- | Given a 'format specification' and a string,
-- split the string into groups of n based on the
-- format spec.
--
-- >>> partitionString [2, 2, 1] "hello"
-- ["he", "ll", "o"]
partitionString :: [Int] -> [a] -> [[a]]
partitionString [] _ = []
partitionString (n:ns) str = fs : partitionString ns bs
    where (fs, bs) = splitAt n str

partitionString' :: [Int] -> [a] -> [[a]]
partitionString' = evalState . traverse (state . splitAt)

-- | Split an integer into sums containing either 1 or 2.
-- Also, the `combos` function does not respect the law of commutativity.
-- hence it includes sums which are merely permutations of one another.
--
-- >>> combos 5
-- [[1,1,1,1,1],[2,1,1,1],[1,2,1,1],[1,1,2,1],[1,1,1,2],[2,2,1],[1,2,2],[2,1,2]]
combos :: Int -> [[Int]]
combos = concatMap permuteMultiset . partitions 2

-- | Generate all possible integer partitions containing
-- a given highest summand.
--
-- >>> partitions 2 5
-- [[1,1,1,1,1],[2,1,1,1],[2,2,1]]
partitions :: Int -> Int -> [[Int]]
partitions _ 0 = [[]]
partitions h n = [a:as | a <- [1 .. min n h], as <- partitions a (n - a)]
