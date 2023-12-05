module Five.App where

import Data.List.Split(splitOn, chunksOf)
import Data.List(find)

type Seed = Int

type Src = Int
type Dest = Int
type Range = Int
type MapEntry = (Dest, Src, Range)
type ConversionMap = [MapEntry]


seeds :: IO ()
seeds = do
  content <- lines <$> readFile "app/Five/input.txt"
  let (seeds, conversionMaps) = parseInput content
  let res = foldr min (maxBound :: Int) . map (seedToLocation conversionMaps) $ seeds
  putStrLn $ "# Task 1: " ++ show res
  let res2 = foldr min (maxBound :: Int) . map (seedRangeToMinLocation conversionMaps) $ map (\[start, num] -> (start, num)) (chunksOf 2 seeds)
  putStrLn $ "# Task 2: " ++ show res2

parseInput :: [String] -> ([Seed], [ConversionMap])
parseInput [] = error "Illegal state: input is empty"
parseInput (l:ls) = let seeds = parseSeeds l
                        maps = map parseMap . filter (not . null) . splitOn [""] $ ls
                    in (seeds, maps)
  where parseSeeds :: String -> [Seed]
        parseSeeds = map read . words . drop (length "seeds: ")

        parseMap :: [String] -> ConversionMap
        parseMap = map (\s -> let [dest, src, range] = words s in (read dest, read src, read range)) . drop 1


seedToLocation :: [ConversionMap] -> Seed -> Dest
seedToLocation [] s = s
seedToLocation (m:ms) s = seedToLocation ms (mapSource m s)

mapSource :: ConversionMap -> Src -> Dest
mapSource m s = let maybeMapping = find (\(_, src, rng) -> s >= src && s < (src + rng)) m
                in maybe s (\(dst, src, _) -> dst + (s - src)) maybeMapping

seedRangeToMinLocation :: [ConversionMap] -> (Seed, Range) -> Dest
seedRangeToMinLocation [] (s, _) = s
seedRangeToMinLocation (m:ms) seed = foldr min (maxBound :: Int) . map (seedRangeToMinLocation ms) $ destChunks m seed
  where destChunks :: ConversionMap -> (Seed, Range) -> [(Dest, Range)]
        destChunks m seedRange = map (\(src, num) -> (mapSource m src, num)) . foldr (\(_, src, rng) ranges -> concat . map (splitRange (src, rng)) $ ranges) [seedRange] $ m 

splitRange :: (Int, Range) -> (Int, Range) -> [(Int, Range)]
splitRange tool@(toolStart, toolNum) range@(start, num) = let intersect = intersection tool range
                                                          in case intersect of 
                                                            Nothing -> [range]
                                                            Just rng@(rngStart, rngNum) -> (if start < rngStart then [(start, rngStart - start)] else []) ++
                                                                                           [rng] ++
                                                                                           (if lastInRange start num > lastInRange rngStart rngNum then [(lastInRange rngStart rngNum + 1, lastInRange start num - lastInRange rngStart rngNum)] else [])

intersection :: (Int, Range) -> (Int, Range) -> Maybe (Int, Range)
intersection (a, ar) (b, br) = let (start, end) = (max a b, min (lastInRange a ar) (lastInRange b br))
                               in if end < start then Nothing else Just (start, end - start + 1)

lastInRange start rng = start + rng - 1
