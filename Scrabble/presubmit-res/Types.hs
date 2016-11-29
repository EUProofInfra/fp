-- Header:

module Types where

import System.Random
import Control.Monad

-- Every item below this will be stripped from the source code, if it exists.
--
-- Items start with "\n-----\n" . Whitespace at start of line will be
-- stripped, as will be empty or comment-only lines.

-- The first line of a fragment starts with "-- ", and forms a
-- descriptive label.

-----
-- type Dict =
type Dict = [String]
-----
-- type Board =
type Board = [[Maybe Char]]
-----
-- type Rack =
type Rack = String
-----
-- type Pos =
type Pos = (Int, Int)
-----
-- data Orient =
data Orient = H | V deriving (Show, Eq, Read)
-----
-- type WordPos =
type WordPos = (Pos, Orient)
-----
-- type Move =
type Move = (String, WordPos)
-----
-- type Score =
type Score = Int
-----
-- data PlayError =
data PlayError = NoFitOnBoard | NotOnRack | NotAWord deriving (Show)
-----
-- type Seed =
type Seed = StdGen
-----
-- newtype LRand a =
newtype LRand a = LRand (Seed -> a)
-----
-- instance Functor LRand
instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)
-----
-- instance Applicative LRand
instance Applicative LRand where
 pure  = return
 (<*>) = ap
-----
-- instance Monad LRand
instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )
-----
-- data Input =
data Input = MoveInput Move | Exit | NewTiles
-----
-- type Template =
type Template = (Char, WordPos, Int, Int)
-----
-- type LetterStream =
type LetterStream = [Char]
-----
-- type FullMove =
type FullMove = Maybe Move
-----
-- type AI =
type AI = Board -> LetterStream -> [FullMove] -> [FullMove]