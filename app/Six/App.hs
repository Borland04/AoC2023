module Six.App where

import Data.List(foldl')
import Control.Applicative((<|>))

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

findNumOfWinWays time distanceToBeat = let firstWinHold = maybe (time `div` 2) id (binSearch searchCmp 1 (time - 1))
                                       in ((time `div` 2) - firstWinHold + 1) * 2 - (if time `mod` 2 == 0 then 1 else 0)
  where searchCmp :: Int -> Ordering
        searchCmp a
          | a * (time - a) <= distanceToBeat = LT
          | (a - 1) * (time - (a - 1)) <= distanceToBeat = EQ
          | otherwise = GT

binSearch :: (Int -> Ordering) -> Int -> Int -> Maybe Int
binSearch comp start end
  | start > end = Nothing
  | start == end = case comp start of
                    EQ -> Just start
                    _  -> Nothing
  | start == end + 1 = case comp start of
                        LT -> Nothing
                        EQ -> Just start
                        GT -> case comp end of
                                EQ -> Just end
                                _  -> Nothing
  | otherwise = let half = (start + end) `div` 2
                in case comp half of
                  EQ -> Just half
                  GT -> binSearch comp start half
                  LT -> binSearch comp (half + 1) end
