{-# LANGUAGE OverloadedStrings #-}
module Main where

import Aang

import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = do
    defaultMain $
        testGroup
            "Tests"
            [ testCase "isPeriodic" (isPeriodic ["X", "Y"] "YXY" @? "?!")
            , testCase
                  "¬isPeriodic"
                  (not (isPeriodic ["X", "Y"] "YXYZ") @? "?!")
            , testProperty "partition 1" $
              \n -> n >= 0 ==> length (partitions 1 n) == 1
            , testProperty "partition 1" $
              \n -> n >= 0 ==> length (head $ partitions 1 n) == n]
    doctest ["src"]
