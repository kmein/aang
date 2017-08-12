module Main (main) where

import Aang

import Control.Monad (forM_)
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import Safe (atMay)
import System.Environment (getArgs)
import Text.CSV (parseCSVFromFile)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- | Get a word as command line argument and output
-- all possible element deconstructions of that word.
--
-- >>> ./aang Hask
-- H As K
main :: IO ()
main = do
    args <- map (Text.toLower . Text.pack) <$> getArgs
    csvPeriodicTable <- parseCSVFromFile "pt-data1.csv"
    case csvPeriodicTable of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right rows ->
            let elements = map Text.toLower $ mapMaybe (fmap Text.pack . (`atMay` 1)) rows
            in forM_ args $
               mapM_ (Text.putStrLn . concatCapitalized) . periods elements

-- | >>> concatCapitalized ["ca","o","s"]
-- "Ca O S"
concatCapitalized :: [Text.Text] -> Text.Text
concatCapitalized = Text.unwords . map capitalize
  where
    capitalize str =
        case Text.uncons str of
            Just (x, xs) -> toUpper x `Text.cons` xs
            Nothing -> Text.empty
