module Three.App where

import Data.Char(isDigit)

data SchematicsAtom = Symbol { symbol :: String, row :: Int, index :: Int }
  | PartNumber { number :: Int, row :: Int, startIndex :: Int, endIndex :: Int }
  deriving Show

readEngineSchematics :: IO ()
readEngineSchematics = do
  putStrLn "Day 3"
  content <- lines <$> readFile "app/Three/input.txt"
  let schematics = parseSchematics content
  putStrLn "Task 1: "
  let res = foldr (+) 0 . map number $ findNumsAdjacentToSymbols schematics
  putStrLn $ show res

parseSchematics :: [String] -> [SchematicsAtom]
parseSchematics = parseSchematics' 0
  where parseSchematics' :: Int -> [String] -> [SchematicsAtom]
        parseSchematics' _ [] = []
        parseSchematics' row (s:ss) = parseLine row 0 s ++ parseSchematics' (row + 1) ss
        parseLine :: Int -> Int -> String -> [SchematicsAtom]
        parseLine _ _ [] = []
        parseLine row currentIndex s@(ch:chs)
          | ch == '.' = parseLine row (currentIndex + 1) chs
          | isDigit ch = let (num, rem) = span isDigit s
                             endIndex = currentIndex + length num - 1
                         in PartNumber (read num :: Int) row currentIndex endIndex : parseLine row (endIndex + 1) rem
          | otherwise = Symbol [ch] row currentIndex : parseLine row (currentIndex + 1) chs


findNumsAdjacentToSymbols :: [SchematicsAtom] -> [SchematicsAtom]
findNumsAdjacentToSymbols schem = let syms = filter (\a -> case a of Symbol _ _ _ -> True; _ -> False) schem
                                      nums = filter (\a -> case a of PartNumber _ _ _ _ -> True; _ -> False) schem
                                  in filter (isAdjacentToSymbol' syms) nums
  where isAdjacentToSymbol' :: [SchematicsAtom] -> SchematicsAtom -> Bool
        isAdjacentToSymbol' syms num = any (flip isAdjacent $ num) syms
        isAdjacent :: SchematicsAtom -> SchematicsAtom -> Bool
        isAdjacent (Symbol _ symRow symIdx) (PartNumber _ numRow startIdx endIdx) = (symRow >= numRow - 1 && symRow <= numRow + 1) && (symIdx >= startIdx - 1 && symIdx <= endIdx + 1)


