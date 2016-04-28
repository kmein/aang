-- | The main module contains all the functionality of the "aang" program.
module Main where

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Math.Combinat.Permutations (permuteMultiset)
import System.Environment (getArgs)

-- | Get a word as command line argument and output
-- all possible element deconstructions of that word.
--
-- >>> ./aang Hask
-- H As K
main :: IO ()
main =
    map (T.toLower . T.pack) <$> getArgs >>=
       mapM_ (mapM_ (T.putStrLn . concatCapitalized) . periods)
    where
      -- | >>> concatCapitalized ["ca","o","s"]
      -- "Ca O S"
      concatCapitalized :: [T.Text] -> T.Text
      concatCapitalized = T.unwords . map capitalize
          where
            capitalize str =
                case T.uncons str of
                  Nothing -> T.empty
                  Just (c, cs) -> toUpper c `T.cons` cs

isPeriodic :: T.Text -> Bool
isPeriodic = not . null . periods . T.toLower

-- | Find all string combos of which all substrings are contained within
-- the element symbol in the periodic table of the elements.
periods :: T.Text -> [[T.Text]]
periods = findPeriodicTableMatches . stringCombos
    where
      findPeriodicTableMatches = filter (all (`elem` periodicTable))
      periodicTable = map T.pack
          ["ac","ag","al","am","ar","as","at","au"
          ,"b","ba","be","bh","bi","bk","br"
          ,"c","ca","cd","ce","cf","cl","cm","co","cr","cs","cu"
          ,"db","ds","dy"
          ,"er","es","eu"
          ,"f","fe","fm","fr"
          ,"ga","gd","ge"
          ,"h","he","hf","hg","ho","hs"
          ,"i","in","ir"
          ,"k","kr"
          ,"la","li","lr","lu"
          ,"md","mg","mn","mo","mt"
          ,"n","na","nb","nd","ne","ni","no","np"
          ,"o","os"
          ,"p","pa","pb","pd","pm","po","pr","pt","pu"
          ,"ra","rb","re","rf","rg","rh","rn","ru"
          ,"s","sb","sc","se","sg","si","sm","sn","sr"
          ,"ta","tb","tc","te","th","ti","tl","tm"
          ,"u"
          ,"v"
          ,"w"
          ,"xe"
          ,"y","yb"
          ,"zn","zr"
          ]

-- | Split a string into all possible groups of 1 and 2.
--
-- >>> stringCombos "word"
-- [["w","o","r","d"],["wo","r","d"],["w","or","d"],["w","o","rd"],["wo","rd"]]
stringCombos :: T.Text -> [[T.Text]]
stringCombos str =
    map (`partitionString` str) $ combos $ T.length str

-- | Given a 'format specification' and a string,
-- split the string into groups of n based on the
-- format spec.
--
-- >>> partitionString [2, 2, 1] "hello"
-- ["he", "ll", "o"]
partitionString :: [Int] -> T.Text -> [T.Text]
partitionString ints str =
    case ints of
      [] -> []
      (n:ns) -> fs : partitionString ns bs
          where (fs, bs) = T.splitAt n str

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
partitions h n =
    if n == 0
       then [[]]
       else [a:as | a <- [1 .. min n h], as <- partitions a (n - a)]

