{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- This file just imports all your exercises.

module Facade where

import Types (Seed(..), LRand(..), Input(..), Board(..), Rack(..), Pos(..), Orient(..), WordPos(..), Move(..), Score(..), Template(..), LetterStream(..), FullMove(..), AI(..), Dict(..), PlayError(..))

import System.Random
import Control.Exception
import Control.Monad
import Control.Monad.State
import Control.DeepSeq
import Data.Char
import Data.Either
import Data.List
import Data.Maybe
import System.Random
import System.Environment
import qualified Scrabble
import Sowpods

boardFromWord :: String -> Board
boardFromWord = Scrabble.boardFromWord
numOcc :: Char -> String -> Int
numOcc = Scrabble.numOcc
submultiset :: String -> String -> Maybe String
submultiset = Scrabble.submultiset
formable :: String -> Rack -> Char -> Maybe String
formable = Scrabble.formable
wordValue :: String -> Score
wordValue = Scrabble.wordValue
invertBoard :: Board -> Board
invertBoard = Scrabble.invertBoard
autoResize :: Board -> Board
autoResize = Scrabble.autoResize
writeMove :: Move -> Board -> Board
writeMove = Scrabble.writeMove
replenishRack :: Rack -> LRand Rack
replenishRack = Scrabble.replenishRack
allWords1 :: Dict -> Char -> [String]
allWords1 = Scrabble.allWords1
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 = Scrabble.allWords2
allWords3 :: Dict -> Rack -> Char -> Int -> Int -> [(String, Int)]
allWords3 = Scrabble.allWords3
letterStream :: LRand LetterStream
letterStream = Scrabble.letterStream
greedyBestMove :: Dict -> Rack -> Board -> FullMove
greedyBestMove = Scrabble.greedyBestMove
ai :: Dict -> AI
ai = Scrabble.ai
connectAI :: Board -> (AI, LetterStream) -> (AI, LetterStream) -> [FullMove]
connectAI = Scrabble.connectAI

type TestAttemptedResult = Either SomeException ()
-- Left means exception, Right means it worked.
testAttempted :: NFData a => a -> IO (Either SomeException ())
testAttempted x = try $ evaluate $ rnf x




exercisesAttempted :: [(String, Int, IO TestAttemptedResult)]
exercisesAttempted = [
    ("boardFromWord", 9,
        testAttempted (boardFromWord "test")),
    ("numOcc", 9,
        testAttempted (numOcc 's' "mississippi")),
    ("submultiset", 4,
        testAttempted (submultiset "aecdb" "bebdcaxa")),
    ("formable", 4,
        testAttempted (formable "exercise" "seeqcixez" 'r')),
    ("wordValue", 9,
        testAttempted (wordValue "scrabol")),
    ("invertBoard", 9,
        testAttempted (invertBoard [[Just 'a', Nothing], [Just 'b', Just 'c']])),
    ("autoResize", 4,
        testAttempted (autoResize [[Just 'a', Nothing], [Just 'b', Just 'c']])),
    ("writeMove", 4,
        testAttempted (writeMove ("de", ((0,0),H)) [[Just 'a', Nothing], [Just 'b', Just 'c']])),
    ("replenishRack", 4,
        testAttempted (show $ runRand (replenishRack "abc") (mkStdGen 42))),
    ("allWords1", 8,
        testAttempted (allWords1 sowpods 'y')),
    ("allWords2", 4,
        testAttempted (allWords2 sowpods 'y' 1 1)),
    ("allWords3", 4,
        testAttempted (allWords3 sowpods "aeo" 'x' 1 1)),
    ("letterStream", 7,
        testAttempted (show $ runRand (liftM (take 100) letterStream) (mkStdGen 42))),
    ("greedyBestMove", 7,
        testAttempted (show $ greedyBestMove sowpods "abcdefg" someBoard)),
    ("ai", 7,
        testAttempted (show $ take 3 $ ai sowpods someBoard (repeat 'a') (replicate 3 Nothing ++ repeat undefined))),
    ("connectAI", 7,
        testAttempted (show $ take 100 $ connectAI someBoard (someAI, "abcdefg") (someAI, "abcdefg")))
  ]

showWhetherAttempted (name, numPts, att) = do
    att <- att
    case att of
      Right () -> putStrLn $ "+     attempted: " ++ name ++ pts
      Left ex -> case (fromException ex :: Maybe SomeAsyncException) of
        Nothing -> putStrLn (
          "- not attempted: "
          ++ name ++ pts
          ++ " (exception: "
          ++ (takeWhile (/= '\n') $ show ex)
          ++ ")"
            )
        Just ex' -> throwIO ex'
  where
    pts = " (" ++ show numPts ++ " points)"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["0"] -> main_compile_ok
    ["1"] -> do 
      putStr "initialising..."
      evaluate $ deepseq sowpods
      putStrLn " done."
      putStrLn ""
      putStrLn "I will now guess which exercises you have attempted."
      putStrLn ""

      mapM showWhetherAttempted exercisesAttempted
      return ()

main_compile_ok :: IO ()
main_compile_ok = putStrLn "Compiled ok."

----- AUXILIARY

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- autoResize (boardFromWord "haskell")
someBoard = [[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'h',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'a',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 's',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'k',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'e',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'l',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 'l',Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]]

someAI _ _ _ = repeat Nothing