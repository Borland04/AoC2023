module Main (main) where

import One.App (readCalibrations)
import Two.App (playCubesGame)


main :: IO ()
main = do
  putStrLn "Day 1"
  readCalibrations "app/One/input.txt"
  putStrLn "Day 2"
  playCubesGame [("red", 12),  ("green", 13), ("blue", 14)] "app/Two/input.txt"
