module Main where

import           Control.Monad
import qualified Data.Map.Strict as Map

import           Lib

main :: IO ()
main = do
  let v = toSudoku sudokuString
      solved = bruteforce v
  forM_ [0 .. 80] $ \i -> do
    when (mod i 9 == 0) (putStr "\n")
    case (Map.!) solved i of
      Nothing -> putStr ". "
      Just i  -> putStr $ show i ++ " "
  putStr "\n"
