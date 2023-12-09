module Nine.App where

ecosystemReport :: IO ()
ecosystemReport = do
  readings <- map (map (read :: String -> Int) . words) . lines <$> readFile "app/Nine/input.txt"
  let diffs = map diffSequences readings
  let res = foldr (+) 0 . map extrapolate $ diffs 
  putStrLn $ "# Task 1: " ++ show res
  let res2 = foldr (+) 0 . map extrapolateBackwards $ diffs 
  putStrLn $ "# Task 2: " ++ show res2


extrapolate :: [[Int]] -> Int
extrapolate = foldr (+) 0 . map last

extrapolateBackwards :: [[Int]] -> Int
extrapolateBackwards = foldr (-) 0 . map head

diffSequences :: [Int] -> [[Int]]
diffSequences nums = let next = process nums
                   in nums : (if all (== 0) next then [next] else diffSequences next)

process :: [Int] -> [Int]
process [] = []
process [_] = []
process (a:b:s) =  (b - a) : process (b:s)
