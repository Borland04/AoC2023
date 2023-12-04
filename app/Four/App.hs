module Four.App where

import Data.Text(pack, unpack, split, strip)
import Data.Foldable(toList)
import Data.List(find)

scratchcards :: IO ()
scratchcards = do
  putStrLn "Day 4"
  content <- lines <$> readFile "app/Four/input.txt"
  let cards = map parseCard content
  putStrLn "Task 1:"
  let res = foldr (+) 0 $ map calculatePoints cards
  putStrLn $ show res
  putStrLn "Task 2:"
  let res2 = foldr (+) 0 . map length $ mapCardsToWinCards cards
  putStrLn $ show res2


parseCard :: String -> ([Int], [Int])
parseCard = numbers . card
  where card :: String -> String
        card = drop 1 . dropWhile (/= ':')

        numbers :: String -> ([Int], [Int])
        numbers s = let [wins, nums] = map words . map unpack . map strip . split (== '|') $ pack s
                    in (map read wins, map read nums)

-- Task 1
calculatePoints :: ([Int], [Int]) -> Int
calculatePoints (wins, nums) = case length $ winNums wins nums of
                                0 -> 0
                                c -> 2 ^ (c - 1)

winNums :: [Int] -> [Int] -> [Int]
winNums _ [] = []
winNums wins (n:ns) = toList (find (== n) wins) ++ winNums wins ns

-- Task 2
mapCardsToWinCards :: [([Int], [Int])] -> [[Int]]
mapCardsToWinCards = doMap 1
  where doMap :: Int -> [([Int], [Int])] -> [[Int]]
        doMap _ [] = []
        doMap idx (c:cs) = let remMap = doMap (idx + 1) cs
                               winCount = length $ winNums (fst c) (snd c) 
                           in (idx : (concat $ take winCount remMap)) : remMap
