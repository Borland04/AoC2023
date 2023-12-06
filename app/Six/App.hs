module Six.App where

import Data.List(foldl')

type Time = Int
type Distance = Int

boatsRace :: IO ()
boatsRace = do
  content <- lines <$> readFile "app/Six/input.txt"
  let doJob = foldr (*) 1 . map (uncurry findNumOfWinWays) 

  let dataSheet = parseInput content
  let res = doJob dataSheet
  putStrLn $ "# Task 1: " ++ show res

  let dataSheet2 = parseInput2 content
  let res2 = doJob dataSheet2
  putStrLn $ "# Task 2: " ++ show res2

parseInput :: [String] -> [(Time, Distance)]
parseInput [timeLine, distanceLine] = zip (parseTime timeLine) (parseDistance distanceLine)
  where parseTime = parseNumsLine . drop (length "Time:")
        parseDistance = parseNumsLine . drop (length "Distance:")
        parseNumsLine :: String -> [Int]
        parseNumsLine = map read . words
parseInput _ = error "Illegal state: expected two lines of input"

parseInput2 :: [String] -> [(Time, Distance)]
parseInput2 [timeLine, distanceLine] = zip (parseTime timeLine) (parseDistance distanceLine)
  where parseTime = parseNumsLine . drop (length "Time:")
        parseDistance = parseNumsLine . drop (length "Distance:")
        parseNumsLine :: String -> [Int]
        parseNumsLine = (: []) . read . filter (/= ' ')
parseInput2 _ = error "Illegal state: expected two lines of input"

findNumOfWinWays time distanceToBeat = foldl' (\acc (a, b) -> acc + (if a == b then 1 else 2)) 0 . filter (\(a, b) -> a * b > distanceToBeat) $ generateWays time
generateWays :: Int -> [(Int, Int)]
generateWays n = [(i, n - i) | i <- [1..(n `div` 2)]]
