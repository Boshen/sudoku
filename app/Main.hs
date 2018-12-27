module Main where

import           Lib

main :: IO ()
main =
  case backtrack (toSudoku sudokuString) of
    Just solved -> putStrLn $ showSudoku solved
    Nothing     -> print "no"
