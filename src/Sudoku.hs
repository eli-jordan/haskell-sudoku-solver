{-# LANGUAGE LambdaCase #-}

module Sudoku where

import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector

import Data.List
import Data.List.Split
import Data.Char
import Data.Either
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Map as Map

-- Allow type to be referred to unqualified
type Matrix = Matrix.Matrix
type Vector = Vector.Vector
type Map = Map.Map

-------------------------------------- Data Structure --------------------------------------

-- The index of a row 0..9
type RowIndex = Int

-- The index of a column 0..9
type ColumnIndex = Int

-- The index of a square. This refers to the linear index of the upper left
-- corner of the square.
type SquareIndex = Int

-- A cell represents a square on the Sudoku grid.
data Cell = Cell {
  -- (row, column) indicates the cells position on the grid
  row :: RowIndex,
  column :: ColumnIndex,

  -- square specifies the identifier of the square the board belongs to
  square :: SquareIndex,

  -- Holds the solved value, or the list of possible values for this cell
  value :: CellValue
} deriving Eq

-- A cell can either be solved, and have a value
-- Or be unsolved and have set of possible values
data CellValue =
    Value Int
  | Unsolved [Int] deriving (Show, Eq)

-- A Sudoku board is just a vector of cells
-- Where the index = row * 9 + col i.e. the rows are just concatenated
newtype Board = Board (Vector Cell) deriving Eq

instance Show Cell where
  show cell =
    let v = show $ value cell
        r = show $ row cell
        c = show $ column cell
    in (v ++ " (" ++ r ++ "," ++ c ++ ")")

instance Show Board where
  show (Board vec) =
    let rows = chunksOf 9 (Vector.toList vec)
    in show $ Matrix.fromLists rows

-- Extracts the specified row from the board
getRow :: RowIndex -> Board -> Vector Cell
getRow = getIndices . getRowIndices
  where getRowIndices :: RowIndex -> [Int]
        getRowIndices row =
          let rowStart = row * 9
              rowEnd = rowStart + 8
          in [rowStart .. rowEnd]

-- Extracts the specified column from the board
getCol :: ColumnIndex -> Board -> Vector Cell
getCol = getIndices . getColumnIndices
  where getColumnIndices :: ColumnIndex -> [Int]
        getColumnIndices col =
          let colStart = col
              colNext = col + 9
              colLast = col + 80
          in [colStart, colNext .. colLast]

-- Get all columns from the board
getCols :: Board -> [[Cell]]
getCols board = fmap (\idx -> Vector.toList $ getCol idx board) [0 .. 8]

-- Get all rows from the board
getRows :: Board -> [[Cell]]
getRows board = fmap (\idx -> Vector.toList $ getRow idx board) [0 .. 8]

-- Get the specified cell from the board
getCell :: RowIndex -> ColumnIndex -> Board -> Cell
getCell row col (Board vec) = vec Vector.! (row * 9 + col)

-- Extracts the specified square frm the board
getSquare :: SquareIndex -> Board -> Vector Cell
getSquare = getIndices . getSquareIndices
  where getSquareIndices :: SquareIndex -> [Int]
        getSquareIndices square =
          let start1 = square
              start2 = square + 9
              start3 = start2 + 9
          in [start1 .. start1 + 2] ++ [start2 .. start2 + 2] ++ [start3 .. start3 + 2]

-- Get all squares from the board
getSquares :: Board -> [[Cell]]
getSquares board = fmap (\idx -> Vector.toList $ getSquare idx board) [0, 3, 6, 27, 30, 33, 54, 57, 60]


getIndices :: [Int] -> Board -> Vector Cell
getIndices indices (Board board) = Vector.fromList $ map (\idx -> board Vector.! idx) indices

-- Set the value of a list of cells onto the board
setCells :: [Cell] -> Board -> Board
setCells cells (Board board) =
  let updates = Vector.fromList $ fmap (\cell -> (getCellIndex cell, cell)) cells
  in Board $ Vector.update board updates


getCellIndex :: Cell -> Int
getCellIndex cell = row cell * 9 + column cell

-------------------------------------- Solver --------------------------------------

-- When there is no deterministic way forwards in the solution
-- takes the cells with the fewest candidates and branches the solution space
-- exploring all branches.
solveMany :: Board -> [Board]
solveMany board = case solve board of
  Right solved -> [solved]
  Left b @ (Board cells) ->
    let sortedUnsolved = filter (not . isCellSolved) (sortBy orderCellsByCandidateCount (Vector.toList cells))
    in do
      cell <- sortedUnsolved
      let candidates = getCandidates cell
      choice <- candidates
      let boardChoice = setCells [cell { value = Value choice }] b
      solveMany boardChoice

-- Compares cells based on the number of candidates, with fewer candidates being lower
orderCellsByCandidateCount :: Cell -> Cell -> Ordering
orderCellsByCandidateCount (Cell _ _ _ (Unsolved vs1)) (Cell _ _ _ (Unsolved vs2)) = compare (length vs1) (length vs2)
orderCellsByCandidateCount (Cell _ _ _ (Value _     )) (Cell _ _ _ (Value _     )) = EQ
orderCellsByCandidateCount (Cell _ _ _ (Unsolved _  )) (Cell _ _ _ (Value _     )) = LT
orderCellsByCandidateCount (Cell _ _ _ (Value _     )) (Cell _ _ _ (Unsolved _  )) = GT


-- Attempts to solve the specified Sudoku board, without
-- making any guesses.
-- returns a Right if the board was solved
-- returns a Left with the partial solution of the board was not solved
solve :: Board -> Either Board Board
solve board @ (Board vec) =
  if isSolved board
  then Right board
  else
    let nextBoard = solveStep board
    in if nextBoard == board
       then Left board
       else solve nextBoard

-- Performs a single step in the deterministic portion of the solution
solveStep :: Board -> Board
solveStep = constrainCells . constrainRows . constrainColumns . constrainSquares

-- Applies cell level constraints to each cell
constrainCells :: Board -> Board
constrainCells board @ (Board vec) = foldr constrainCell board vec

-- Applies constraints to a cell to reduce the number of available possibilities
constrainCell :: Cell -> Board -> Board
constrainCell cell board =
  case value cell of
    Unsolved [x] -> setCells [cell {value = Value x}] board
    Unsolved _ ->
      let rowVec = Vector.toList (getRow (row cell) board)
          colVec = Vector.toList (getCol (column cell) board)
          squareVec = Vector.toList (getSquare (square cell) board)

          consistencyC = consistencyConstraint cell (rowVec, colVec, squareVec)
          hiddenSingleC = hiddenSingleConstraint consistencyC (rowVec, colVec, squareVec)
      in setCells [hiddenSingleC] board
    _ -> board

-- Reduces the candidates based on analysis of a group i.e. row, column, square
-- Currently just applies the "Naked Pairs" strategy
constrainGroups :: [[Cell]] -> Board -> Board
constrainGroups group = setCells (group >>= nakedPairConstraint)

constrainRows :: Board -> Board
constrainRows board = constrainGroups (getRows board) board

constrainColumns :: Board -> Board
constrainColumns board = constrainGroups (getCols board) board

constrainSquares :: Board -> Board
constrainSquares board = constrainGroups (getSquares board) board

-- Strategies from http://www.angusj.com/sudoku/hints.php

-- Algorithm 1: Singles (Cell)
--    let P = possible values for the cell
--    let R = solved values in the row
--    let C = solved values in the column
--    let S = solved values in the square
--
--    setCellValue P - R - C - S
consistencyConstraint :: Cell -> ([Cell], [Cell], [Cell]) -> Cell
consistencyConstraint cell (row, col, square) =
  let options = getCandidates cell
      solvedRowVec = row >>= getSolvedValue
      solvedColVec = col >>= getSolvedValue
      solvedSquareVec = square >>= getSolvedValue

      constrained = options \\ (solvedRowVec ++ solvedColVec ++ solvedSquareVec)
  in
      cell { value = Unsolved constrained }

-- Algorithm 2: Hidden singles (Cell)
--    let P = possible values for the cell
--    let Z = possible values for all cells in a row or column or square
--    if count(P - Z) == 1 ==> The remaining element is the cells value
hiddenSingleConstraint :: Cell -> ([Cell], [Cell], [Cell]) -> Cell
hiddenSingleConstraint cell (row, col, square) =
  let rowOptions = filter (/= cell) row >>= getCandidates
      colOptions = filter (/= cell) col >>= getCandidates
      sqOptions  = filter (/= cell) square >>= getCandidates

      cellOptions = getCandidates cell

      hiddenRow = cellOptions \\ rowOptions
      hiddenCol = cellOptions \\ colOptions
      hiddenSq  = cellOptions \\ sqOptions
  in
    if length hiddenRow == 1
    then cell { value = Value $ head hiddenRow }
    else if length hiddenCol == 1
    then cell { value = Value $ head hiddenCol }
    else if length hiddenSq == 1
    then cell { value = Value $ head hiddenSq }
    else cell


-- Algorithm 3: Naked Pairs (Group)
--   If there are 2 cells with the same 2 candidates in a group, then
--   one cell must have the first value, and the other cell the second value
--   Therefore, we can remove these candidates from all other cells in the group
nakedPairConstraint :: [Cell] -> [Cell]
nakedPairConstraint group =
  let pairs = filter (\c -> length (getCandidates c) == 2) group
      eqCandidates c1 c2 = sort (getCandidates c1) == sort (getCandidates c2)
      duplicates = filter (\d -> length d == 2) (findDuplicates eqCandidates pairs)
  in duplicates >>= (\case [x, y] -> updatedCells (x, y) group)
  where
    updatedCells :: (Cell, Cell) -> [Cell] -> [Cell]
    updatedCells (p1, p2) group =
      let pairValues = getCandidates p1
          neighbours = filter (not . isCellSolved) (delete p1 (delete p2 group))
      in fmap (\n -> n {value = Unsolved (getCandidates n \\ pairValues)}) neighbours

groupBy :: Ord k => (v -> k) -> [v] -> Map k [v]
groupBy key as = Map.fromListWith (++)  as' where
            as' = map ((,) <$> key <*> (:[])) as

-- Find any duplicate values in the specified list,
-- comparing elements using the provide function
findDuplicates :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
findDuplicates _ [] = []
findDuplicates f (x : xs) =
  let dupes = filter (f x) (x : xs)
      remain = xs \\ dupes
  in case dupes of
        [] -> findDuplicates f remain
        _ -> dupes : findDuplicates f remain

-- Determines whether the board is solved
isSolved :: Board -> Bool
isSolved (Board board) = Vector.all isCellSolved board

-- Determines whether a single cell is solved
isCellSolved (Cell _ _ _ (Value _)) = True
isCellSolved _ = False

getAllValues :: CellValue -> [Int]
getAllValues (Value v) = [v]
getAllValues (Unsolved vs) = vs

getCandidates :: Cell -> [Int]
getCandidates (Cell _ _ _ (Unsolved v)) = v
getCandidates _ = []

-- Get the value of a cell
getSolvedValue :: Cell -> [Int]
getSolvedValue (Cell _ _ _ (Value v)) = [v]
getSolvedValue _ = []



-------------------------------------- Interface --------------------------------------

solvePuzzleList :: String -> IO ()
solvePuzzleList file = do
  fileData <- readFile file
  let boards = zip [1 ..] (fmap parseBoard (lines fileData))
  solved <- traverse solveBoard boards
  putStrLn ("Solved Count" ++ show (sum solved))


solveBoard :: (Int, Board) -> IO Int
solveBoard (idx, board) = do
  putStrLn ("Solving puzzle " ++ show idx)
  let solved = not $ null $ solveMany board
--  putStrLn ("Solved?" ++ show solved)
  if solved
    then return 1
    else do
      putStrLn ("Failed to solve puzzle " ++ show idx)
      return 0

-- Converts a string representing a Sudoku board (where '0' indicates an unsolved
-- cell) into the Board data structure.
parseBoard :: String -> Board
parseBoard input =
  let asDigits :: String -> [Int]
      asDigits = map digitToInt

      rows = chunksOf 9 (asDigits (Text.unpack $ Text.strip $ Text.pack input))

      rowsWithIndex = zip [0 ..] rows
  in
      Board $ Vector.fromList $ rowsWithIndex >>= uncurry parseRow

-- Parses a single row of the board
parseRow :: RowIndex -> [Int] -> [Cell]
parseRow row values =
  let toCell :: (ColumnIndex, Int) -> Cell
      toCell (col, value) =
        if value == 0
        then Cell row col (computeSquareIndex row col) (Unsolved [1..9])
        else Cell row col (computeSquareIndex row col) (Value value)

      valuesWithIndex = zip [0..] values
  in
    fmap toCell valuesWithIndex

-- Calculate the index of the upper left cell in a square, based on the row and column of a cell
computeSquareIndex :: RowIndex -> ColumnIndex -> SquareIndex
computeSquareIndex row col =
  let rowBound = bounds row
      colBound = bounds col

      bounds :: Int -> Int
      bounds x =
         let xf = fromIntegral (x + 1)
             upper = ceiling (xf / 3) * 3
             lower = upper - 2
         in lower - 1
  in
     rowBound * 9 + colBound

doSolve str = case (solve . parseBoard) str of
  Left board -> board
  Right board -> board

solveList0 = solvePuzzleList "./puzzles/0.txt"
solveList1 = solvePuzzleList "./puzzles/1.txt"
solveList2 = solvePuzzleList "./puzzles/2.txt"
solveList3 = solvePuzzleList "./puzzles/3.txt"
solve17HintsList = solvePuzzleList "./puzzles/sudoku17-ml.txt"