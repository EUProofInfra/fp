{-# LANGUAGE CPP #-}

module Scrabble where

import Control.Monad
import Control.DeepSeq
import Data.Char
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Random

import Sowpods

import qualified Blank as Bram

{-

This includes parts 1 and 2 of the 2nd assessed exercise. An advanced
set for high 1st class is missing and will be given in the second week
of the exercise. With the current set here, you will be able to get a
low 1st class mark.

--------------------------------------------------------------------
  Preparation for the exercise
--------------------------------------------------------------------

(1) First install the sample solutions module with command

    $ sh install.sh

This will create some subdirectories that you don't need to look at
(but you may).

(2) This will allows you to run this file with the command

    $ ghci -package-db=Bram/ghc Scrabble

(3) At the moment, your exercises below are solved by ourselves, so
that you can run them and see what output is expected. You will need
to replace lines "Bram.*" by your own code (or keep them if you are
not solving a particular exercise). All exercises are labeled by the
word "exercise" in a comment.

(4) Also we provide below code for you to use.

(5) In the first deadline, we expect you to submit this file with
about half of the questions answered.

(6) In the second and last deadline, you should submit this file again,
with all the work you managed to do.

(7) There will be an additional set of advanced exercises to get a
high 1st class mark. With this set, you will be able to get a low 1st
class mark.

--------------------------------------------------------------------
  The Exercise - Scrabble
--------------------------------------------------------------------

The exercise is based on the game Scrabble aka Words with Friends.

Scrabble is quite a complicated game to implement, so we change a few
rules to simplify it. We call the new game "SCRABOL". This game is
very similar to Scrabble, with a couple of exceptions listed below. In
particular, there are no blanks and parallel words are forbidden.

That is, in Scrabble but not in SCRABOL, you can make the following
play (indicated in uppercase):


      b
      e
      t
      t
    W e
    A r
    X
    Y


In Scrabble this is valid because the following three words are valid
words: WE, AR, WAXY. But in our game, SCRABOL, this is not valid. In
SCRABOL, only the following kind of play is allowed:

      b
      e
      t
      t
    W e A T H E R
      r

Or:

      b
      e
      t
      t
      e
    W r I T T E N

(Parallel play example adapted from
http://blog.xjtian.com/post/51339158829/a-smarter-scrabble-ai-part-2-move-validation .)

If you are not familiar with Scrabble, have a go online a bit to get a feel
for the game, for instance at https://www.lexulous.com/ . (Warning: in
Lexulous, you have 8 letters on your rack, whereas we always have 7.) You can
also run the following command in ghci to play against the sample solution:

    playAgainstYourself sowpods "helloworld"

The rules of Scrabble are given here: http://scrabble.hasbro.com/en-us/rules .
The rules of SCRABOL are the same, with these exceptions, to make the game simpler:

Setup
    The game consists of an infinite board and any number of players. The
    dictionary will be given to you.

rule #1 (about playing words):
    For SCRABOL, parallel words are not allowed. That is, all words on the
    board stay unchanged, and exactly one word is added, intersecting with
    exactly one existing tile on the board.

rule #2 (about completing a turn):
    We don't draw the letter tiles from a fixed pool of 100 tiles, but the
    computer randomly generates tiles (using the same frequency).

rule #6 (about blanks)
    goes away, we don't have blanks.

rule #8 (about challenging a play)
    becomes irrelevant; word validity is checked by the computer.

rule #9 (about ending a game) becomes
    The game ends after a predetermined number of turns. We will determine
    this number later, it depends on how fast our program will run.

-}

type Dict = [String]

-- There is a dictionary already provided, under variable name "sowpods". It
-- is one of the official Scrabble dictionaries used in Europe.

-- Normalise a dictionary to Scrabble-like words, by removing accents,
-- throwing out proper names, removing punctuation, and nubbing.

normDict :: Dict -> Dict
normDict = filter valid
  where
    valid word = all (`elem` aToZ) word
    aToZ = "abcdefghijklmnopqrstuvwxyz"

{-

A board is represented by a list of lists, where each list is a
column. Each column is a list of Maybe Char, where Nothing represents
a vacant position, and Just c represents a position occupied by the
character c. All the columns should be of equal length.

-}

type Board = [[Maybe Char]]

{-

A rack contains the list of tiles you can play, represented as a list
of characters, that is, as a String. The order of the characters
doesn't matter.

-}

type Rack = String
rackSize = 7 :: Int

-- Positions, as (x, y). As you go down, y increases; as you go right, x
-- increases:
type Pos = (Int, Int)

-- An orientation can be horizontal (H) or vertical (V):
data Orient = H | V deriving (Show, Eq, Read)

-- A word position is a start position together with an orientation:
type WordPos = (Pos, Orient)

-- A move is a word (represented as a String) together with a position:
type Move = (String, WordPos)

-- A score is represented as an Int:
type Score = Int

-- Find a tile on the board. Utility code give to you:
onBoard :: Pos -> Board -> Maybe Char
onBoard (x, y) b = if withinBounds then b !! x !! y else Nothing
  where
    withinBounds = 0 <= x && x < length b
                && 0 <= y && y < length (head b)

-- Whether a non-initial move is valid on a given Board.

-- In SCRABOL, this is when two conditions hold.
--
--   1) There is exactly one tile where the word is supposed to go. That tile
--      should match the corresponding letter in the word. We call this
--      position the "intersection position".
--
--   2) Consider the positions directly adjacent to where the word is supposed
--      to go on the north, east, south, and west side (we ignore the
--      corners). Those positions must be empty, with two exceptions. If the
--      word goes horizontally, then the positions just north and south of the
--      intersection position may additionally be occupied. If the word goes
--      vertically, then the positions just west and east of the intersection
--      position may additionally be occupied.

-- Utility code give to you:
validMove :: Move -> Board -> Bool
validMove move@(w, (pos, orient)) b
    = uniqueIntersect && noConflicts && enoughRoom
  where
    intersectionPoss = [pos | pos <- movePositions move, pos `onBoard` b /= Nothing]
    uniqueIntersect = length intersectionPoss == 1
    intersectionPos :: Pos
    intersectionPos = head intersectionPoss
    (ix, iy) = intersectionPos
    intersectionLetter :: Char
    intersectionLetter = fromJust (moveLetter move intersectionPos)
    noConflicts = all noConflict (movePositions move)
    noConflict pos'
        | intersectionPos == pos'
            = (pos' `onBoard` b) == Just intersectionLetter
        | otherwise
            = (pos' `onBoard` b) == Nothing
    allowedAdjPos :: [Pos]
    allowedAdjPos
      | orient == H = [(ix, iy-1), (ix, iy+1)]
      | orient == V = [(ix-1, iy), (ix+1, iy)]
    enoughRoom = all okAdjPos (adjacentToWord move)
    okAdjPos adjPos = adjPos `elem` allowedAdjPos || (adjPos `onBoard` b) == Nothing

-- Utility code give to you:
movePositions :: Move -> [Pos]
movePositions (w, ((x, y), H)) = [(x + i, y) | i <- [0..length w - 1]]
movePositions (w, ((x, y), V)) = [(x, y + i) | i <- [0..length w - 1]]

-- Utility code give to you:
adjacentToWord :: Move -> [Pos]
adjacentToWord (w, ((x, y), H))
    = [(x-1, y), (x + length w, y)]
      ++ concat [[(x+i, y-1), (x+i, y+1)] | i <- [0..length w - 1]]
-- A vertical move is just a horizontal move if you mirror the board around
-- the line y=x.
adjacentToWord (w, ((x, y), V)) = map transposePos (adjacentToWord (w, ((y, x), H)))

-- Utility code give to you:
transposePos (x, y) = (y, x)
transposeOrient V = H
transposeOrient H = V
transposeWPos :: WordPos -> WordPos
transposeWPos ((x, y), o) = ((y, x), transposeOrient o)


-- Given one of the positions of a move, compute the letter that should go
-- there. Slow implementation. Utility code give to you:
moveLetter :: Move -> Pos -> Maybe Char
moveLetter move@(w, ((x, y), _)) (x', y') = do
    guard $ (x', y') `elem` movePositions move
    let offset = x' - x + y' - y
    return (w !! offset)


-- Exercise, basic. Make a function that, given a word, creates a board with
-- only that word on, horizontally.
--
-- boardFromWord "test" = [[Just 't'],[Just 'e'],[Just 's'],[Just 't']]
--
-- Tip: you may use the function 'transpose' from Data.List.

boardFromWord :: String -> Board
boardFromWord = Bram.boardFromWord

-- Exercise, basic. Count the number of occurrences of a character in a string.
--
-- numocc 'c' "abccbedcce" = 4

numOcc :: Char -> String -> Int
numOcc = Bram.numOcc

-- Exercise, medium. Given two words, determine whether you can make
-- the left word with the letters of the right word. You do not need
-- to use all letters of the right word. If it does work, return a
-- string with the leftover letters. They do not need to be in order.
--
-- submultiset "aecdb" "bebdcaxa" = Just "xba"
-- submultiset "aecdeb" "bebdcaa" = Nothing
--
-- Hint: look at Data.List, there are useful set functions there.

submultiset :: String -> String -> Maybe String
submultiset = Bram.submultiset

-- Exercise, medium. Given a word, a list of letters on your rack, and the
-- intersection point letter c, determine whether you can form the word on the
-- board on that intersection point by adding letters from your rack. If so,
-- return the letters that are left over. They do not need to be in order.
--
-- formable "exercise" "seeqcixez" 'r' = Just "qz"
-- formable "exercise" "seeqcixez" 'x' = Nothing

formable :: String -> Rack -> Char -> Maybe String
formable = Bram.formable

-- Utility code give to you:
letterValue :: Char -> Score
letterValue c | c `elem` "aeioulnstr" = 1
letterValue c | c `elem` "dg" = 2
letterValue c | c `elem` "bcmp" = 3
letterValue c | c `elem` "fhvwy" = 4
letterValue c | c `elem` "k" = 5
letterValue c | c `elem` "jx" = 8
letterValue c | c `elem` "qz" = 10
letterValue c | otherwise = error "not a valid letter"

-- Exercise, basic. Make a function to compute the value of a word for
-- SCRABOL. Remember that SCRABOL, unlike Scrabble, does not have premium
-- squares: the value of a word in SCRABOL is simply the sum of the values of
-- the individual letters.

wordValue :: String -> Score
wordValue = Bram.wordValue

-- Exercise, basic.
--
-- Given a board, rotate it 180 degrees. The resulting board has the same
-- number of rows and columns as the input.
invertBoard :: Board -> Board
invertBoard = Bram.invertBoard


-- Exercise, hard.
--
-- Given a board, add as many blank rows and columns on either side to make
-- sure new words will always fit. That is, you must add blank rows and
-- columns on the top, left, right, and bottom, until there are at least 7
-- blank rows and columns on all sides.
--
-- The resulting Board will have precisely 7 blank rows and columns on all
-- sides, unless there were more blank rows and columns to start with.
--
-- We recommend that you experiment with the sample solution (Bram.autoResize)
-- to see what is expected.

autoResize :: Board -> Board
autoResize = Bram.autoResize

-- The following errors may occur when attempting to play:
data PlayError = NoFitOnBoard | NotOnRack | NotAWord deriving (Show)

-- Utility code give to you:
playMove :: Rack -> Move -> Dict -> Board -> Either PlayError (Board, Rack, Score)
playMove rack move@(w, ((x, y), orient)) dict b = do
    -- Do-syntax for the Either monad!

    when (not (w `elem` dict)) $ Left NotAWord

    -- If the move doesn't fit, abort.
    when (not $ validMove move b) (Left NoFitOnBoard)

    let [intersectionPos] = [pos | pos <- movePositions move, pos `onBoard` b /= Nothing]
    let Just intersectionLetter = moveLetter move intersectionPos

    rackLeft <- case formable w rack intersectionLetter of
        Nothing -> Left NotOnRack
        Just whatsleft -> return whatsleft


    return (writeMove move b, rackLeft, wordValue w)

-- Exercise, medium/hard. Given a board and a move that is to be executed on
-- that board, put the move on the board and return the resulting board. You
-- can assume that the given move is valid on the given board.

writeMove :: Move -> Board -> Board
writeMove = Bram.writeMove . read . show

-- Exercise, medium/hard. We now move to randomly adding letters to the rack.
--
-- There is a predefined monad for randomness. However, we are going
-- to use our own monad, for the sake of practice (also, our monad
-- works better with laziness, but this is not exploited in this
-- exercise).
--
--   https://hackage.haskell.org/package/MonadRandomLazy-0.1/docs/Control-Monad-LazyRandom.html
--
-- instead, defined below.


-- We use the standard random generator as our type of seeds for
-- random things:

type Seed = StdGen

-- We get seeds for random-thing generation from Int's:

mkSeed :: Int -> Seed
mkSeed = mkStdGen

-- See https://en.wikipedia.org/wiki/Random_seed
-- We define the monad as follows:

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )

-- The following are to "get out" this monad:

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

-- What this says is that if you have a random element of type a (that
-- is, something of type LRand a), one way to get something of type a
-- is to provide a seed.

-- This is like the above, but also produces a new seed, if we need it:

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- If we have access to IO, we can generate a new seed, use it, and throw away the modified seed:
runRandIO :: LRand a -> IO a
runRandIO gen = do
  seed <- newStdGen
  let (value, _) = runRand gen seed
  return value

-- And finally we need to be able to generate random elements:

getRandom :: Random a => LRand a
getRandom = LRand $ fst . random

-- But this needs to be in the Random type class. Most types are
-- automatically there, and it is unlikely you will need to worry
-- about this in this exercise, unless you do very sophisticated
-- things.

-- We also may need to get random elements within a range:

getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range

-- This is the end of our definition of our lazy randomness monad.

pickUniformly :: [a] -> LRand a
pickUniformly xs = do
   n <- getRandomR (0, length xs - 1)
   return(xs !! n)

-- Utility function.
-- Sample a random value from a weighted list.  The total weight of all
-- elements must not be 0.
fromList :: [(a,Rational)] -> LRand a
fromList [] = error "fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRandomR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs

-- Utility data give to you:
letterFrequencies :: [(Char, Rational)]
letterFrequencies = [
        ('a', 9), ('b', 2), ('c', 2), ('d', 4), ('e', 12), ('f', 2), ('g', 3),
        ('h', 2), ('i', 9), ('j', 1), ('k', 1), ('l', 4), ('m', 2), ('n', 6),
        ('o', 8), ('p', 2), ('q', 1), ('r', 6), ('s', 4), ('t', 6), ('u', 4),
        ('v', 2), ('w', 2), ('x', 1), ('y', 2), ('z', 1)
    ]

-- Exercise, medium, no points. Randomly pick a letter between a to z for
-- adding to a rack. Letter e should be more frequent than q; frequencies
-- should be as given in letterFrequencies.
--
-- We suggest that you make newLetter, because it will make replenishRack
-- below easier to implement.
newLetter :: LRand Char
newLetter = let Bram.LRand f = Bram.newLetter in LRand f

-- Exercise, medium/hard. Given a rack, randomly fill it with random
-- letters, to have rackSize many letters.
replenishRack :: Rack -> LRand Rack
replenishRack rack = let Bram.LRand f = Bram.replenishRack rack in LRand f

-- Code for visually testing your solution: let you play against yourself.
--
--     > playAgainstYourself sowpods <any string>

playAgainstYourself :: Dict -> String -> IO Board
playAgainstYourself dict initialWord = playAgainstYourself' dict "" initialBoard
  where
    initialBoard = boardFromWord $ map toLower initialWord

-- The above uses the following data type and helper functions:
data Input = MoveInput Move | Exit | NewTiles

-- Helper function:
playAgainstYourself' :: Dict -> Rack -> Board -> IO Board
playAgainstYourself' dict rack board = do
    let board' = autoResize board
    rack' <- runRandIO (replenishRack rack)
    putStrLn ""
    printBoard board'
    putStrLn ""
    putStrLn $ "Your rack contains:   " ++ intersperse ' ' rack'
    mmove <- getInput
    let retry = playAgainstYourself' dict rack' board
    case mmove of
        Exit -> putStrLn "Thanks for playing!" >> return board'
        NewTiles -> playAgainstYourself' dict "" board'
        MoveInput move -> do
            case playMove rack' move dict board' of
                Left NoFitOnBoard ->
                    putStrLn "That doesn't make a valid SCRABOL shape." >> retry
                Left NotOnRack ->
                    putStrLn "You don't have the tiles to do that." >> retry
                Left NotAWord ->
                    putStrLn ("I can't find \"" ++ fst move ++ "\" in my dictionary.") >> retry
                Right (board, rack', score) -> do
                    putStrLn $ "You scored " ++ show score ++ " points!"
                    playAgainstYourself' dict rack' board

-- Helper function:
getInput :: IO Input
getInput = do
    putStrLn "Please enter a move. Example: (\"word\", ((4, 0), H)) to put"
    putStrLn "the word on the top row. Or type exit or newtiles."
    line <- getLineFancy "Your move> "
    case line of
      "exit" -> return Exit
      "newtiles" -> return NewTiles
      _ -> case reads line :: [(Move, String)] of
            [] -> putStrLn "Invalid syntax." >> getInput
            ((move, _):_) -> return (MoveInput move)


-- Helper function:
-- Like readline, but EOF becomes "exit", and automatically builds history.
getLineFancy :: String -> IO String
getLineFancy prompt = do
    putStr prompt
    getLine

-- A combination of intersection point, orientation, and how many spaces are
-- free before/after that point to lay tiles on.
type Template = (Char, WordPos, Int, Int)

-- Utility function:
templates :: Board -> [Template]
templates b = templatesV b ++ templatesH b

-- Helper function:
templatesV :: Board -> [Template]
templatesV b = do
    -- Uses the list monad.
    x <- [0..width-1]
    y <- [0..height-1]
    -- Pick only positions with a character
    char <- maybeToList ((x, y) `onBoard` b)
    -- Could this be a valid intersection point? Only if (x, y-1) and (x, y+1)
    -- are free.
    guard (isFree (x, y-1))
    guard (isFree (x, y+1))

    -- For every tile (x,y') we want to lay on top, we need (x, y'-1), (x-1,
    -- y'), and (x, y'+1) to be empty too.
    let canLayOnTop y' = isFree (x, y'-1) && isFree (x-1,y') && isFree (x+1,y')
    -- Similar for bottom.
    let canLayOnBottom y' = isFree (x, y'+1) && isFree (x-1,y') && isFree (x+1,y')
    let validYsOnTop = takeWhile canLayOnTop [y-1,y-2..y-maxExtension]
    let validYsOnBottom = takeWhile canLayOnBottom [y+1,y+2..y+maxExtension]
    let numOnTop = length validYsOnTop
    let numOnBottom = length validYsOnBottom

    guard (numOnTop > 0 || numOnBottom > 0)

    return (char, ((x, y), V), numOnTop, numOnBottom)

  where
    isFree pos = pos `onBoard` b == Nothing
    maxExtension = rackSize
    width = length b
    height = length (head b)

-- Helper function:
templatesH b = map transposeTemp (templatesV (transpose b))

-- Helper function:
transposeTemp :: Template -> Template
transposeTemp (c, wpos, before, after)
    = (c, transposeWPos wpos, before, after)

-- Exercise, basic. Give all the words in the dictionary that contain a
-- certain letter. Word order does not matter.

allWords1 :: Dict -> Char -> [String]
allWords1 = Bram.allWords1

-- Exercise, hard.
--
-- Say that we have the following row on our board:
--
--     X - - - - - - - E - - - - - - X
--
-- We're wondering which words would fit there, overlapping with the
-- E. The word 'elsewhere' has four Es, and fits in two ways, namely
-- with the middle two E's. (As always in SCRABOL, when you play a
-- word they must overlap with exactly one tile on the board, so
-- 'xenophobe' would not fit here.)
--
-- Given an "intersection letter" (in this case E), and the number of
-- free spaces around it, find all the words that would fit in such a
-- space. Also, give all the positions where that letter is anchored;
-- in the example, 'elsewhere' can be anchored at positions 3 or 6.
--
-- In this example, the number of free spaces is 6 and 5,
-- respectively, because we must leave some space around the Xes.
--
-- allWords2 dict 'e' 6 5 = [..., ("elsewhere", 3), ("elsewhere", 6"), ...]
--
-- allWords2 dict 'x' 1 2 = [("ax", 2), ("axe", 2), ("axed", 2),
--    ("axes", 2), ("axis", 2), ("axle", 2), ("axon", 2), ("ex", 2), ("exam",
--    2), ("exec", 2), ("exes", 2), ("exit", 2), ("expo", 2), ("ox", 2),
--    ("oxen", 2), ("x", 1)]
--
-- (You may give the words in a different order.)

allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 = Bram.allWords2

-- Exercise, medium.
--
-- Same, but only consider words that can be made using the given rack.
-- (You may give the words in a different order.)

allWords3 :: Dict -> Rack -> Char -> Int -> Int -> [(String, Int)]
allWords3 = Bram.allWords3


-- Utility code, for manual testing:
printBoard :: Board -> IO ()
printBoard b = checks `seq` actuallyPrint
  where
    width = length b
    height = length (head b)
    checks = checkRectangular
    checkRectangular = all (== height) (map length b)
    transpB = transpose b
    actuallyPrint = do
        putStrLn $ "   " ++ concat [" " ++ show2digs i | i <- [0..width-1]]
        mapM_ printRow (transpB `zip` [0..])
    printRow (row, y) = do
        putStrLn $ " " ++ show2digs y ++ concat ["  " ++ showField c | c <- row]
    showField Nothing = "-"
    showField (Just c) = [c]

-- A right-aligned digit under 100.
show2digs n | 0 <= 0 && n < 10 = " " ++ show n
show2digs n | 0 <= 0 && n < 100 = show n


