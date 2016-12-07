{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
module TestHelpers where


import Types
import safe qualified Scrabble (boardFromWord, numOcc, submultiset, formable, wordValue, allWords1, allWords2, allWords3, autoResize, invertBoard, writeMove, replenishRack, letterStream, greedyBestMove, ai, connectAI)

import Control.Exception
import Control.Monad.State
import Data.Maybe
import Data.List
import System.Random
import ReducedSowpods




func_numOcc :: (Char, String) -> Int
func_numOcc = uncurry Scrabble.numOcc
func_submultiset :: (String, String) -> Maybe String
func_submultiset = fmap sort . uncurry Scrabble.submultiset
func_formable :: (String, Rack, Char) -> Maybe String
func_formable = fmap sort . uncurry3 Scrabble.formable
func_writeMove :: (Move, Board) -> Board
func_writeMove = uncurry Scrabble.writeMove
func_replenishRack_head_length :: Rack -> Int
func_replenishRack_head_length r = num_equal_elem r (evalRand (Scrabble.replenishRack r) ourSeed) -- to be checked
func_replenishRack_length :: Rack -> Int
func_replenishRack_length r = length (evalRand (Scrabble.replenishRack r) ourSeed) -- to be checked
test_replenishRack_distribution :: () -> ()
test_replenishRack_distribution () = flip assert () $ flip evalRand ourSeed $ do
    let genRack = do
        rack <- Scrabble.replenishRack "test"
        return (rack \\ "test")
    ls <- sequence $ repeat genRack
    let initial_segment = take 1000 $ concat ls
    return $ chi_square_h0_twosided_below_p letterFrequencies initial_segment chi_square_p

func_allWords1 :: (Char) -> [String]
func_allWords1 c = sort $ Scrabble.allWords1 reduced_sowpods c
func_allWords2 :: (Char, Int, Int) -> [(String, Int)]
func_allWords2 (c, a, b) = sort $ Scrabble.allWords2 reduced_sowpods c a b
func_allWords3 :: (Rack, Char, Int, Int) -> [(String, Int)]
func_allWords3 (r,c,a,b) = sort $ Scrabble.allWords3 reduced_sowpods r c a b

-- Test that the letters in a single letterStream are appropriately distributed
test_letterStream_distribution :: () -> ()
test_letterStream_distribution () = flip assert () $ flip evalRand ourSeed $ do
    ls <- Scrabble.letterStream
    let initial_segment = take 1000 ls
    return $ chi_square_h0_twosided_below_p letterFrequencies initial_segment chi_square_p

-- Test that the initial segments are appropriately distributed.
test_letterStream_independent :: () -> ()
test_letterStream_independent () = flip assert () $ flip evalRand ourSeed $ do
    lss <- sequence $ repeat Scrabble.letterStream
    let bits = map (take 5) lss
    let initial_segment = take 1000 $ concat bits
    return $ chi_square_h0_twosided_below_p letterFrequencies initial_segment chi_square_p

ourSeed = mkStdGen 54321

func_greedyBestMove :: (Rack, Board) -> Int
func_greedyBestMove (r,b) = moveScore $ Scrabble.greedyBestMove reduced_sowpods r b -- to be checked

test_ai :: String -> ()
test_ai initialWord = test_some_ai initialWord (Scrabble.ai reduced_sowpods)
-- test_ai initialWord = test_some_ai initialWord (test_poop_ai)

test_poop_ai _ _ _ = repeat $ Just ("poop", ((1,1),H))


-- Play 15 moves, verify that they were alright & locally optimal.
test_some_ai :: String -> AI -> ()
test_some_ai initialWord proponent = seq (augmented !! 15) ()
  where
    augmented = func_some_ai initialWord proponent

func_some_ai :: String -> AI -> [(Board, Rack, Rack, FullMove, LetterStream, Board, Board)]
func_some_ai initialWord proponent = flip evalRand ourSeed $ do
    ls1 <- letterStream
    ls2 <- letterStream
    let b = autoResize $ boardFromWord initialWord
    let opponent = aiStupid 19 reduced_sowpods
    let allMoves = connectAI b (proponent, ls1) (opponent, ls2)
    return $ augmentAI reduced_sowpods b "" ls1 allMoves

-- Return at each move of proponent (1st player): initial board, initial rack,
-- move played, letterstream after move, board after move, board after opp
-- move. Asserts that moves were valid and optimal.
augmentAI :: Dict -> Board -> Rack -> LetterStream -> [FullMove] -> [(Board, Rack, Rack, FullMove, LetterStream, Board, Board)]
augmentAI d b1 r1 ls1 (Nothing:oppMove:moves) = assert noMove $ ((b1, r2, r3, Nothing, ls3, b1, b2) : augmentAI d b2 r3 ls3 moves)
  where
    b2 = writeFullMoveResize oppMove b1
    lettersNeeded = rackSize - length r1
    (r2, ls2) = (r1 ++ take lettersNeeded ls1, drop lettersNeeded ls1)
    r3 = (take 7 ls2)
    ls3 = (drop 7 ls2)
    noMove = Nothing == greedyBestMove d r2 b1

augmentAI d b1 r1 ls1 (Just move:oppMove:moves) =
    let
        was_resized = b1 == autoResize b1
        lettersNeeded = rackSize - length r1
        (r2, ls2) = (r1 ++ take lettersNeeded ls1, drop lettersNeeded ls1)
        errorMsg = ("illegal move: " ++ show (r2, move, b1))
        (b3, r3, s_obtained) = fromRight' errorMsg $ playMove r2 move d b1
        s_optimal = moveScore (greedyBestMove d r2 b1)
        was_optimal = s_obtained == s_optimal
        b4 = autoResize b3
        b5 = writeFullMoveResize oppMove b4

    in assert (was_resized && was_optimal) ((b1, r2, r3, Just move, ls2, b4, b5) : augmentAI d b5 r3 ls2 moves)

fromRight' msg (Left x) = error $ msg ++ ": " ++ show x
fromRight' _ (Right y) = y

writeFullMoveResize Nothing b = b
writeFullMoveResize (Just m) b = autoResize $ writeMove m b

func_connectAI :: String -> [FullMove]
func_connectAI initialWord = flip evalRand ourSeed $ do
    ls1 <- letterStream
    ls2 <- letterStream
    let infMoves = Scrabble.connectAI b (aiStupid 3 reduced_sowpods, ls1) (aiStupid 17 reduced_sowpods, ls2)
    return $ take 40 infMoves
  where
    b = autoResize $ boardFromWord initialWord


-- helper functions
list_equality :: Eq a => [a] -> [a] -> Bool
list_equality [] [] = True
list_equality [] (y:ys) = False
list_equality (x:xs) [] = False
list_equality (x:xs) ys = (b) && (list_equality xs zs)
  where
    (b, zs) = member x ys

member :: Eq a => a -> [a] -> (Bool, [a])
member a l = aux l []
  where
    aux [] ys = (False, ys)
    aux (x:xs) ys = if a == x then (True, ys ++ xs) else aux xs (x:ys)

num_equal_elem :: Eq a => [a] -> [a] -> Int
num_equal_elem []     ys = 0
num_equal_elem (x:xs) ys = if b then 1 + num_equal_elem xs ys' else num_equal_elem xs ys
  where
    (b, ys') = member x ys

uncurry3 f (a,b,c) = f a b c
uncurry4 f (a,b,c,d) = f a b c d
uncurry5 f (a,b,c,d,e) = f a b c d e


----- For comparing distributions


type Distr a = [(a, Rational)]

-- Makes sure the frequencies add up to 1.
normalize :: Eq a => Distr a -> Distr a
normalize xs = assert allUnique $ map (\(x, y) -> (x, y/total)) xs
  where
    total = sum $ map snd xs
    allUnique = elements == nub elements
    elements = map fst xs

-- Number of occurrences of an element in a list.
occ :: (Eq a, Num num) => a -> [a] -> num
occ x = sum . map (const 1) . filter (==x)

sq x = x * x

-- Test-statistic for Pearson's chi-square test. Compares distributions.
chi_square_statistic :: Eq a => Distr a -> [a] -> Rational
chi_square_statistic freqs xs
    = n * sum [(sq (occ elt xs / n - p)) / p | (elt,p) <- distr]
  where
    n = fromIntegral $ length xs
    distr = normalize freqs

chi_square_inverse_cdf :: Rational -> Int -> Rational
-- Approximately. According to Wolfram|Alpha.

-- For n == 100, cdf(55) is really more about 0.998
chi_square_inverse_cdf 0.9995 25 = 55
-- For n == 1000, empirically, cdf(4.8) < 0.0001 . (0.003% in 30000 random trials)
-- For n == 1000, empirically, cdf(70) > 0.9999 . (100% in 30000 random trials)
--
-- Expect to falsely fail <0.16 students on at least one test (two-sided;
-- assume 200 students and 4 tests).
chi_square_inverse_cdf 0.000004 25 = 4.8
chi_square_inverse_cdf 0.999996 25 = 70

-- guesswork to lower the odds.
chi_square_inverse_cdf 0.0000004 25 = 0.05
chi_square_inverse_cdf 0.9999996 25 = 90


chi_square_inverse_cdf p degrees_of_freedom = undefined

chi_square_h0_twosided_below_p :: Eq a => Distr a -> [a] -> Rational -> Bool
chi_square_h0_twosided_below_p freqs xs p = min_statistic < statistic && statistic < max_statistic
  where
    statistic = chi_square_statistic freqs xs
    min_statistic = chi_square_inverse_cdf p degrees_of_freedom
    max_statistic = chi_square_inverse_cdf (1-p) degrees_of_freedom
    degrees_of_freedom = length freqs - 1

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

moveScore :: FullMove -> Score
moveScore Nothing = 0
moveScore (Just (w, _)) = wordValue w


chi_square_p :: Rational
chi_square_p = 0.0000004 -- see TestHelpers.hs


-- Instead of the best move, picks the k'th move.
greedyStupidMove :: Int -> Dict -> Rack -> Board -> FullMove
greedyStupidMove k dict rack b = last $ take k (allMoves dict rack b)


aiStupidOneMove :: Int ->  Dict -> State (Board, Rack, LetterStream, [FullMove]) FullMove
aiStupidOneMove k dict = do
    (b, rack, ls, oppMove:oppMoves) <- get
    let toTake = rackSize - length rack
    let rack' = rack ++ take toTake ls
    let ls' = drop toTake ls

    let fullMove = greedyStupidMove k dict rack' b
    -- TODO: wtf: why doesn't this compile? (x2)
    -- let (b2, r2, _)
    --   = case fullMove of
    --       Nothing -> (b, "", 0)
    --       Just move -> fromRight $ playMove rack' move dict b
    let (b2, r2, _) = case fullMove of
        { Nothing -> (b, "", 0) ;
          Just move -> fromRight $ playMove rack' move dict b }
    let b3 = case oppMove of
        { Nothing -> b2 ;
          Just oppMov -> writeMove oppMov (autoResize b2) }
    put (autoResize b3, r2, ls', oppMoves)
    checkBoard b $ return fullMove
  where
    checkBoard b x =
      if b /= autoResize b
      then error "Bram.aiStupidOneMove: error: input board was not autoResized"
      else x

aiStupid :: Int -> Dict -> AI
aiStupid k dict b ls oppMoves = flip evalState (b, [], ls, oppMoves) $ sequence $ repeat $ aiStupidOneMove k dict







--
-- HERE COME SAMPLE SOLUTIONS; needed for testing
--

allMoves :: Dict -> Rack -> Board -> [FullMove]
allMoves dict rack b = [Nothing] ++ properMoves
  where
    properMoves :: [FullMove]
    properMoves = do
        (c, ((x, y), orient), before, after) <- templates b
        (word, anchor) <- allWords3 dict rack c before after
        let (x', y') = shift (x, y) anchor orient
        return (Just (word, ((x', y'), orient)))

    shift (x, y) k H = (x-k, y)
    shift (x, y) k V = (x, y-k)
playMove :: Rack -> Move -> Dict -> Board -> Either PlayError (Board, Rack, Score)
playMove rack move@(w, ((x, y), orient)) dict b = do
    when (not (w `elem` dict)) $ Left NotAWord
    when (not $ validMove move b) (Left NoFitOnBoard)

    let [intersectionPos] = [pos | pos <- movePositions move, pos `onBoard` b /= Nothing]
    let Just intersectionLetter = moveLetter move intersectionPos

    rackLeft <- case formable w rack intersectionLetter of
        Nothing -> Left NotOnRack
        Just whatsleft -> return whatsleft
    return (writeMove move b, rackLeft, wordValue w)
rackSize = 7 :: Int
fromRight (Right r) = r
fromRight _ = error "fromRight: expected a Right!"
templates :: Board -> [Template]
templates b = templatesV b ++ templatesH b
templatesV :: Board -> [Template]
templatesV b = do
    x <- [0..width-1]
    y <- [0..height-1]
    char <- maybeToList ((x, y) `onBoard` b)
    guard (isFree (x, y-1))
    guard (isFree (x, y+1))
    let canLayOnTop y' = isFree (x, y'-1) && isFree (x-1,y') && isFree (x+1,y')
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
templatesH b = map transposeTemp (templatesV (transpose b))
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
movePositions :: Move -> [Pos]
movePositions (w, ((x, y), H)) = [(x + i, y) | i <- [0..length w - 1]]
movePositions (w, ((x, y), V)) = [(x, y + i) | i <- [0..length w - 1]]
onBoard :: Pos -> Board -> Maybe Char
onBoard (x, y) b = if withinBounds then b !! x !! y else Nothing
  where
    withinBounds = 0 <= x && x < length b
                && 0 <= y && y < length (head b)
moveLetter :: Move -> Pos -> Maybe Char
moveLetter move@(w, ((x, y), _)) (x', y') = do
    guard $ (x', y') `elem` movePositions move
    let offset = x' - x + y' - y
    return (w !! offset)
transposeTemp :: Template -> Template
transposeTemp (c, wpos, before, after)
    = (c, transposeWPos wpos, before, after)
adjacentToWord :: Move -> [Pos]
adjacentToWord (w, ((x, y), H))
    = [(x-1, y), (x + length w, y)]
      ++ concat [[(x+i, y-1), (x+i, y+1)] | i <- [0..length w - 1]]
adjacentToWord (w, ((x, y), V)) = map transposePos (adjacentToWord (w, ((y, x), H)))
transposeOrient V = H
transposeOrient H = V
transposeWPos :: WordPos -> WordPos
transposeWPos ((x, y), o) = ((y, x), transposeOrient o)
transposePos (x, y) = (y, x)
connectAI :: Board -> (AI, LetterStream) -> (AI, LetterStream) -> [FullMove]
connectAI b (ai1, ls1) (ai2, ls2) = mergeLists moves1 moves2
  where
    moves1 = ai1 b ls1 moves2
    secondBoard = case (head moves1) of
        Nothing -> b
        Just move -> writeMove move b
    moves2 = ai2 (autoResize secondBoard) ls2 (tail moves1)

writeMove :: Move -> Board -> Board
writeMove (w, ((x, y), V)) b = leftRows ++ [writtenRow] ++ rightRows
  where
    leftRows = take x b
    rightRows = drop (x+1) b
    sourceActiveRow = b !! x
    writtenRow = topFields ++ map Just w ++ bottomFields
    topFields = take y sourceActiveRow
    bottomFields = drop (y + length w) sourceActiveRow
writeMove (w, ((x, y), H)) b = transpose $ writeMove (w, ((y, x), V)) $ transpose b
mergeLists :: [a] -> [a] -> [a]
mergeLists (x:xs) ys = x : mergeLists ys xs
mergeLists [] ys = ys
wordValue :: String -> Score
wordValue = sum . map letterValue
autoResize :: Board -> Board
autoResize = autoResizeNW . invertBoard . autoResizeNW . invertBoard
autoResizeNW = autoResizeW . autoResizeN
autoResizeW b = blankColumns ++ b
  where
    spaceNeeded = rackSize
    blankColumns = replicate newColumnsNeeded blankColumn
    blankColumn = replicate height Nothing
    height = length $ head b
    columnEmpty :: [Bool]
    columnEmpty = map (all (==Nothing)) b
    columnsEmpty :: Int
    columnsEmpty = length $ takeWhile (==True) columnEmpty
    newColumnsNeeded = spaceNeeded - columnsEmpty `max` 0
autoResizeN b = map (blankCells++) b
  where
    spaceNeeded = rackSize
    blankCells = replicate newRowsNeeded Nothing
    rowsEmpty = minimum (map emptyPrefixLenInColumn b)
    emptyPrefixLenInColumn col = length $ takeWhile (==Nothing) col
    newRowsNeeded = spaceNeeded - rowsEmpty `max` 0
letterStream :: LRand LetterStream
letterStream = do
  letter <- newLetter
  rest <- letterStream
  return (letter : rest)
boardFromWord :: String -> Board
boardFromWord w = transpose [map Just w]
allWords2 :: Dict -> Char -> Int -> Int -> [(String, Int)]
allWords2 d c before after = do
    word <- d
    guard (c `elem` word) -- not needed, but 2x faster
    (wordLetter, intersectionPos) <- zip word [0..]
    guard (c == wordLetter)
    guard (intersectionPos <= before)
    guard (length word - intersectionPos - 1 <= after)
    return (word, intersectionPos)
allWords3 :: Dict -> Rack -> Char -> Int -> Int -> [(String, Int)]
allWords3 d rack c before after = do
    (w, pos) <- allWords2 d c before after
    guard $ formable w rack c /= Nothing
    return (w, pos)
letterValue :: Char -> Score
letterValue c | c `elem` "aeioulnstr" = 1
letterValue c | c `elem` "dg" = 2
letterValue c | c `elem` "bcmp" = 3
letterValue c | c `elem` "fhvwy" = 4
letterValue c | c `elem` "k" = 5
letterValue c | c `elem` "jx" = 8
letterValue c | c `elem` "qz" = 10
letterValue c | otherwise = error "letterValue: error: not a valid letter"
invertBoard :: Board -> Board
invertBoard = reverse . map reverse
newLetter :: LRand Char
newLetter = fromList letterFrequencies
letterFrequencies :: [(Char, Rational)]
letterFrequencies = [
        ('a', 9), ('b', 2), ('c', 2), ('d', 4), ('e', 12), ('f', 2), ('g', 3),
        ('h', 2), ('i', 9), ('j', 1), ('k', 1), ('l', 4), ('m', 2), ('n', 6),
        ('o', 8), ('p', 2), ('q', 1), ('r', 6), ('s', 4), ('t', 6), ('u', 4),
        ('v', 2), ('w', 2), ('x', 1), ('y', 2), ('z', 1)
    ]
formable :: String -> Rack -> Char -> Maybe String
formable word rack c = submultiset word (rack ++ [c])
fromList :: [(a,Rational)] -> LRand a
fromList [] = error "fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do
  let s = (fromRational (sum (map snd xs))) :: Double -- total weight
      cs = scanl1 (\(_,q) (y,s') -> (y, s'+q)) xs       -- cumulative weight
  p <- liftM toRational $ getRandomR (0.0,s)
  return . fst . head $ dropWhile (\(_,q) -> q < p) cs
submultiset :: String -> String -> Maybe String
submultiset xs ys | (xs \\ ys) == [] = Just (ys \\ xs)
submultiset xs ys | otherwise        = Nothing
getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range
greedyBestMove :: Dict -> Rack -> Board -> FullMove
greedyBestMove dict rack b = maximumBy orderMove (allMoves dict rack b)
orderMove :: FullMove -> FullMove -> Ordering
orderMove m1 m2 = moveScore m1 `compare` moveScore m2
