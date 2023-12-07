module Seven.App where

import Data.List(sortOn, sortBy)
import Data.Ord(Down(..))

camelCards :: IO ()
camelCards = do
  content <- lines <$> readFile "app/Seven/input.txt"
  let input = parseInput content
  let res = calculateWinnings input
  putStrLn $ "# Task 1: " ++ show res

parseInput :: [String] -> [([Card], Int)]
parseInput = map parseLine
  where parseLine s = let [hand, bid] = words s in (hand, read bid)

calculateWinnings :: [([Card], Int)] -> Int
calculateWinnings = foldr (\(rank, (_, bid)) acc -> acc + (bid * rank)) 0 . zip [1..] . orderHands

orderHands :: [([Card], Int)] -> [([Card], Int)]
orderHands hands = map (\((h, _), b) -> (h, b)) . sortBy (\a b -> compareHands (fst a) (fst b)) . map (\(h, b) -> ((h, sortOn (Down . length) (groupCards h)), b)) $ hands


type Card = Char
cards :: [Card]
cards = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']

handTypes :: [([[Card]] -> Bool)]
handTypes = [fiveOfKind, fourOfKind, fullHouse, threeOfKind, twoPair, onePair, highCard]

-- Note, Important: order by group size DESC before calling these methods

fiveOfKind :: [[Card]] -> Bool
fiveOfKind grps = length grps == 1

fourOfKind :: [[Card]] -> Bool
fourOfKind [a, _] = length a == 4
fourOfKind _ = False

fullHouse :: [[Card]] -> Bool
fullHouse [a, _] = length a == 3
fullHouse _ = False

threeOfKind :: [[Card]] -> Bool
threeOfKind [a, _, _] = length a == 3
threeOfKind _ = False

twoPair :: [[Card]] -> Bool
twoPair [a, b, _] = length a == 2 && length b == 2
twoPair _ = False

onePair :: [[Card]] -> Bool
onePair [a, _, _, _] = length a == 2
onePair _ = False

highCard :: [[Card]] -> Bool
highCard [_, _, _, _, _] = True
highCard _ = False

groupCards :: [Card] -> [[Card]]
groupCards = foldr addToGroup [] 
  where addToGroup :: Card -> [[Card]] -> [[Card]]
        addToGroup c [] = [[c]]
        addToGroup c (a:as) = if head a == c then (c:a) : as else a : addToGroup c as


compareHands :: ([Card], [[Card]]) -> ([Card], [[Card]]) -> Ordering
compareHands a b = compareHands' a b handTypes
  where compareHands' a b (t:ts) = case (t (snd a), t (snd b)) of
                                    (True, False) -> GT
                                    (False, True) -> LT
                                    (True, True) -> compareSameTypeHands (fst a) (fst b)
                                    (False, False) -> compareHands' a b ts
        compareHands' a b [] = compareSameTypeHands (fst a) (fst b)


compareSameTypeHands :: [Card] -> [Card] -> Ordering
compareSameTypeHands _ [] = EQ
compareSameTypeHands [] _ = EQ
compareSameTypeHands (a:as) (b:bs) = case compareCards a b of
                                      EQ -> compareSameTypeHands as bs
                                      other -> other

compareCards :: Card -> Card -> Ordering
compareCards a b = compareCards' a b cards
  where compareCards' a b [] = error $ "Illegal state: one of the cards is unknown: " ++ [a] ++ " or " ++ [b]
        compareCards' a b (c:cs)
          | a == b = EQ
          | a == c = LT
          | b == c = GT
          | otherwise = compareCards' a b cs