-- | This module contains the basic functionality of the »aang« program.
module Aang where

import Math.Combinat.Permutations (permuteMultiset)
import qualified Data.Text as Text

isPeriodic :: [Text.Text] -> Text.Text -> Bool
isPeriodic tbl = not . null . periods tbl . Text.toLower

-- | Find all string combos of which all substrings are contained within
-- the element symbol in the periodic table of the elements.
periods :: [Text.Text] -> Text.Text -> [[Text.Text]]
periods periodicTable = filter (all (`elem` periodicTable)) . stringCombos

-- | Split a string into all possible groups of 1 and 2.
--
-- >>> stringCombos (Text.pack "word")
-- [["w","o","r","d"],["w","o","rd"],["w","or","d"],["wo","r","d"],["wo","rd"]]
stringCombos :: Text.Text -> [[Text.Text]]
stringCombos str = map (`partitionString` str) $ combos $ Text.length str

-- | Given a 'format specification' and a string,
-- split the string into groups of n based on the
-- format spec.
--
-- >>> partitionString [2, 2, 1] (Text.pack "hello")
-- ["he","ll","o"]
partitionString :: [Int] -> Text.Text -> [Text.Text]
partitionString [] _ = []
partitionString (n:ns) str = fs : partitionString ns bs
    where (fs, bs) = Text.splitAt n str

-- partitionString' :: [Int] -> [a] -> [[a]]
-- partitionString' = evalState . traverse (state . splitAt)

-- | Split an integer into sums containing either 1 or 2.
-- Also, the `combos` function does not respect the law of commutativity.
-- hence it includes sums which are merely permutations of one another.
--
-- >>> combos 5
-- [[1,1,1,1,1],[1,1,1,2],[1,1,2,1],[1,2,1,1],[2,1,1,1],[1,2,2],[2,1,2],[2,2,1]]
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
