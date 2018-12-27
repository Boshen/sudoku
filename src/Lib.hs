module Lib where

import           Control.Parallel.Strategies (parList, rseq, using)
import           Data.Char                   (digitToInt)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

type Cell = Maybe Int

type Sudoku = Map Int Cell

sudokuString = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

-- sudokuString = "364978512152436978879125634738651429691247385245389167923764851486512793517893246"
toSudoku :: String -> Sudoku
toSudoku s =
  let list =
        flip map s $ \c ->
          if c == '.'
            then Nothing
            else Just (digitToInt c)
      tuples = zip [0 .. 80] list
   in Map.fromList tuples

bruteforce :: Sudoku -> Sudoku
bruteforce = head . backtrack

backtrack :: Sudoku -> [Sudoku]
backtrack s
  | solved s = [s]
  | reject s = []
  | otherwise = concat (backtrack <$> children s `using` parList rseq)

reject :: Sudoku -> Bool
reject s = Map.size (Map.filter (Nothing /=) s) == 81

children :: Sudoku -> [Sudoku]
children s = do
  i <- [1 .. 9]
  return $ Map.insert nextEmptyIndex (Just i) s
  where
    nextEmptyIndex = fst . head . Map.toList $ Map.filter (Nothing ==) s

solved :: Sudoku -> Bool
solved sudoku =
  let results = map (Map.restrictKeys sudoku) indices
      sizes = map (Set.size . Set.fromList . mapMaybe snd . Map.toList) results
   in all (== 9) sizes

indices :: [Set Int]
indices = map Set.fromList (rows ++ cols ++ boxes)
  where
    rows = [map ((i * 9) +) [0 .. 8] | i <- [0 .. 8]]
    cols = [map (j +) [i | i <- [0 .. 80], mod i 9 == 0] | j <- [0 .. 8]]
    boxes = [[i * 9 + j | j <- [m .. (m + 2)], i <- [0 .. 2]] | m <- [0, 3, 6, 27, 30, 33, 54, 57, 60]]
