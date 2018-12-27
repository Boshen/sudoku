module Lib where

import           Data.Char   (digitToInt)
import           Data.Maybe
import qualified Data.Set    as Set
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Debug.Trace

type Cell = Maybe Int

type Sudoku = Vector Cell

-- sudokuString = "................................................................................."
sudokuString = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

-- backtrack native 10s
-- with pruning 0.5s
-- sudokuString = "36497.5121524369.887912.634738.514296912473852453.916792376.85148651279351789324."
toSudoku :: String -> Sudoku
toSudoku s =
  flip V.map (V.fromList s) $ \c ->
    if c == '.'
      then Nothing
      else Just (digitToInt c)

backtrack :: Sudoku -> Maybe Sudoku
backtrack = go
  where
    go x =
      if solved x
        then Just x
        else listToMaybe . catMaybes . V.toList $ (go <$> children x)

children :: Sudoku -> Vector Sudoku
children s =
  let allIndices = V.filter ((Nothing ==) . snd) (V.indexed s)
   in if null allIndices
        then V.empty
        else let nextIndex = fst . V.head $ allIndices
                 digits = Set.fromList . catMaybes . concatMap (map ((V.!) s)) $ filter (nextIndex `elem`) indices
                 posibilities = V.fromList . Set.toList $ Set.difference (Set.fromList [1 .. 9]) digits
              in V.map (\i -> (V.//) s [(nextIndex, Just i)]) posibilities

solved :: Sudoku -> Bool
solved sudoku =
  if Nothing `elem` sudoku
    then False
    else let results = map (map ((V.!) sudoku)) indices
             sizes = map (Set.size . Set.fromList) results
          in all (== 9) sizes

indices :: [[Int]]
indices = rows ++ cols ++ boxes
  where
    rows = [map ((i * 9) +) [0 .. 8] | i <- [0 .. 8]]
    cols = [map (j +) [i | i <- [0 .. 80], mod i 9 == 0] | j <- [0 .. 8]]
    boxes = [[i * 9 + j | j <- [m .. (m + 2)], i <- [0 .. 2]] | m <- [0, 3, 6, 27, 30, 33, 54, 57, 60]]

showSudoku :: Sudoku -> String
showSudoku sudoku =
  concat $
  flip map [0 .. 80] $ \i ->
    let n =
          case (V.!) sudoku i of
            Just v  -> show v ++ " "
            Nothing -> "  "
     in n ++
        if (i + 1) `mod` 9 == 0
          then "\n"
          else ""
