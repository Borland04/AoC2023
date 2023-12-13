module Main (main) where

import One.App (readCalibrations)
import Two.App (playCubesGame)
import Three.App (readEngineSchematics)
import Four.App (scratchcards)
import Five.App (seeds)
import Six.App (boatsRace)
import Seven.App(camelCards)
import Eight.App(navigateDesert)
import Nine.App(ecosystemReport)
import Ten.App(findAnimal)
import Eleven.App(galaxiesDistance)


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
  putStrLn ""
  putStrLn "-- Day 5 --"
  seeds
  putStrLn ""
  putStrLn "-- Day 6 --"
  boatsRace
  putStrLn ""
  putStrLn "-- Day 7 --"
  camelCards
  putStrLn ""
  putStrLn "-- Day 8 --"
  navigateDesert
  putStrLn ""
  putStrLn "-- Day 9 --"
  ecosystemReport
  putStrLn "-- Day 10 --"
  findAnimal
  putStrLn "-- Day 11 --"
  galaxiesDistance
