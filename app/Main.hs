module Main (main) where

import Aang

import Control.Monad (forM_)
import Data.Char (toLower, toUpper)
import Data.Maybe (mapMaybe)
import Safe (atMay)
import System.Environment (getArgs)
import Text.CSV (parseCSVFromFile)

-- | Get a word as command line argument and output
-- all possible element deconstructions of that word.
--
-- >>> ./aang Hask
-- H As K
main :: IO ()
main =
    do args <- map (map toLower) <$> getArgs
       csvPeriodicTable <- parseCSVFromFile "pt-data1.csv"
       case csvPeriodicTable of
         Left err -> putStrLn $ "Parse error: " ++ show err
         Right rows | elements <- map (map toLower) $ mapMaybe (`atMay` 1) rows ->
             forM_ args $ \arg ->
                 mapM_ (putStrLn . concatCapitalized) $
                 periods elements arg

-- | >>> concatCapitalized ["ca","o","s"]
-- "Ca O S"
concatCapitalized :: [String] -> String
concatCapitalized = unwords . map capitalize
  where
    capitalize [] = []
    capitalize (x:xs) = toUpper x : xs
