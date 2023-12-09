module Nine.App where

ecosystemReport :: IO ()
ecosystemReport = do
  readings <- map (map (read :: String -> Int) . words) . lines <$> readFile "app/Nine/input.txt"
  let res = foldr (+) 0 . map extrapolate . map diffSequences $ readings
  putStrLn $ "# Task 1: " ++ show res

extrapolate :: [[Int]] -> Int
extrapolate = foldr (+) 0 . map last

diffSequences :: [Int] -> [[Int]]
diffSequences nums = let next = process nums
                   in nums : (if all (== 0) next then [next] else diffSequences next)

process :: [Int] -> [Int]
process [] = []
process [_] = []
process (a:b:s) =  (b - a) : process (b:s)
