{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

module Blank (module Types, boardFromWord, numOcc, submultiset, formable, wordValue, invertBoard, autoResize, writeMove, newLetter, replenishRack, allWords1, allWords2, allWords3, letterStream, greedyBestMove, aiOneMove, ai, connectAI
    ) where

import Types


import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import System.Random

boardFromWord :: String -> Board
boardFromWord x1 = undefined
numOcc :: Char -> String -> Int
numOcc x1 x2 = undefined
submultiset :: String -> String -> Maybe String
submultiset x1 x2 = undefined
formable :: String -> Rack -> Char -> Maybe String
formable x1 x2 x3 = undefined
wordValue :: String -> Score
wordValue x1 = undefined
invertBoard :: Board -> Board
invertBoard x1 = undefined
autoResize :: Board -> Board
autoResize x1 = undefined
writeMove :: Move -> Board -> Board
writeMove x1 x2 = undefined
newLetter :: LRand Char
newLetter = undefined
replenishRack :: Rack -> LRand Rack
replenishRack x1 = undefined
allWords1 :: Dict -> Char -> [String]
allWords1 x1 x2 = undefined
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 _ 'x' 1 1 = [("ax",1),("axe",1),("ex",1),("exo",1),("ox",1),("oxo",1),("oxy",1),("xi",0),("xu",0)]
allWords2 x1 x2 x3 x4 = undefined
allWords3 :: Dict -> Rack -> Char -> Int -> Int -> [(String, Int)]
allWords3 x1 x2 x3 x4 x5 = undefined
letterStream :: LRand LetterStream
letterStream = undefined
greedyBestMove :: Dict -> Rack -> Board -> FullMove
greedyBestMove x1 x2 x3 = undefined
aiOneMove :: Dict -> State (Board, Rack, LetterStream, [FullMove]) FullMove
aiOneMove x1 = undefined
ai :: Dict -> AI
ai x1 = undefined
connectAI :: Board -> (AI, LetterStream) -> (AI, LetterStream) -> [FullMove]
connectAI x1 x2 x3 = undefined
