module Main (main) where

import One.App (readCalibrations)

main :: IO ()
main = do
  readCalibrations "app/One/input.txt"
