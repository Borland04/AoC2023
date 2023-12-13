module Eleven.App where

import Debug.Trace(traceShow)

galaxy = '#'
space = '.'

galaxiesDistance :: IO ()
galaxiesDistance = do
  galaxyMap <- lines <$> readFile "app/Eleven/input.txt"
  let galaxies = findGalaxies galaxyMap
  let galaxyPairs = generateVariants galaxies
  let expRows = expandedRows galaxyMap
  let expCols = expandedCols galaxyMap
  let res = sum $ map (uncurry $ dist 2 expRows expCols) galaxyPairs
  putStrLn $ "# Task 1: " ++ show res
  let res2 = sum $ map (uncurry $ dist 1000000 expRows expCols) galaxyPairs
  putStrLn $ "# Task 2: " ++ show res2

expandedRows :: [String] -> [Int]
expandedRows [] = []
expandedRows m = filter (\r -> all (== space) (m !! r)) [0..(length m - 1)]

expandedCols :: [String] -> [Int]
expandedCols [] = []
expandedCols m =  filter (\c -> all (\r -> m !! r !! c == space) [0..length m - 1]) $ [0..(length (m !! 0) - 1)]

findGalaxies :: [String] -> [(Int, Int)]
findGalaxies m = [(r, c) | r <- [0..length m - 1],
                           c <- [0..length (m !! 0) - 1],
                           m !! r !! c == galaxy]

generateVariants :: [a] -> [(a, a)]
generateVariants [] = []
generateVariants (a : as) = map ((,) a) as ++ generateVariants as

dist :: Int -> [Int] -> [Int] -> (Int, Int) -> (Int, Int) -> Int
dist expFactor expRows expCols (r1, c1) (r2, c2) = let (tr, br, lc, rc) = (min r1 r2, max r1 r2, min c1 c2, max c1 c2) 
                                                       expRowsCount = length $ filter (flip elem expRows) [tr..br]
                                                       expColsCount = length $ filter (flip elem expCols) [lc..rc]
                                                   in (expRowsCount + expColsCount) * expFactor + (((br - tr) - expRowsCount) + ((rc - lc) - expColsCount))
