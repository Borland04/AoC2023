module Main (main) where

import One.App (readCalibrations)
import Two.App (playCubesGame)
import Three.App (readEngineSchematics)
import Four.App (scratchcards)


main :: IO ()
main = do
  putStrLn "-- Day 1 --"
  readCalibrations
  putStrLn ""
  putStrLn "-- Day 2 --"
  playCubesGame
  putStrLn ""
  putStrLn "-- Day 3 --"
  readEngineSchematics
  putStrLn ""
  putStrLn "-- Day 4 --"
  scratchcards
