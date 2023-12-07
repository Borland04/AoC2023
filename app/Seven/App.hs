module Seven.App where

import Data.List(sortOn, sortBy)
import Data.Ord(Down(..))

data HandType = HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Show, Eq, Ord)

type Card = Char
cards :: [Card]
cards = [ '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
cards2 :: [Card]
cards2 = [ 'J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A']

handTypes :: [(HandType, [[Card]] -> Bool)]
handTypes = [(FiveOfKind, fiveOfKind2), (FourOfKind, fourOfKind2), (FullHouse, fullHouse2), (ThreeOfKind, threeOfKind2), (TwoPair, twoPair2), (OnePair, onePair2)]

camelCards :: IO ()
camelCards = do
  content <- lines <$> readFile "app/Seven/input.txt"
  let input = parseInput content
  let res = calculateWinnings input
  putStrLn $ "# Task 1: " ++ show res
  let res2 = calculateWinnings2 input
  putStrLn $ "# Task 2: " ++ show res2

parseInput :: [String] -> [([Card], Int)]
parseInput = map parseLine
  where parseLine s = let [hand, bid] = words s in (hand, read bid)

-- Task 1

calculateWinnings :: [([Card], Int)] -> Int
calculateWinnings = foldr (\(rank, (_, bid)) acc -> acc + (bid * rank)) 0 . zip [1..] . orderHands

orderHands :: [([Card], Int)] -> [([Card], Int)]
orderHands hands = map snd . sortBy (\(ta, (ha, _)) (tb, (hb, _)) -> compareHands cards (ta, ha) (tb, hb)) . map (\(h, b) -> (handType h, (h, b))) $ hands

handType :: [Card] -> HandType
handType h = handType' handTypes $ sortOn (Down . length) $ groupCards h
  where handType' ((t, f):ts) grp = if f grp then t else handType' ts grp
        handType' [] _ = HighCard


-- Task 2

calculateWinnings2 :: [([Card], Int)] -> Int
calculateWinnings2 = foldr (\(rank, (_, bid)) acc -> acc + (bid * rank)) 0 . zip [1..] . orderHands2

orderHands2 :: [([Card], Int)] -> [([Card], Int)]
orderHands2 hands = map snd . sortBy (\(ta, (ha, _)) (tb, (hb, _)) -> compareHands cards2 (ta, ha) (tb, hb)) . map (\(h, b) -> (handType2 h, (h, b))) $ hands

handType2 :: [Card] -> HandType
handType2 = handType . filter (/= 'J')


-- Note, Important: order by group size DESC before calling these methods
-- Note, Important: highly rely on fact, that hand has 5 cards

fiveOfKind2 :: [[Card]] -> Bool
fiveOfKind2 [_] = True
fiveOfKind2 [] = True
fiveOfKind2 _ = False

fourOfKind2 :: [[Card]] -> Bool
fourOfKind2 [_, [_]] = True
fourOfKind2 _ = False

fullHouse2 :: [[Card]] -> Bool
fullHouse2 [_, _] = True
fullHouse2 _ = False

threeOfKind2 :: [[Card]] -> Bool
threeOfKind2 [_, [_], [_]] = True
threeOfKind2 _ = False

twoPair2 :: [[Card]] -> Bool
twoPair2 [_, _, [_]] = True
twoPair2 _ = False

onePair2 :: [[Card]] -> Bool
onePair2 [_, [_], [_], [_]] = True
onePair2 _ = False


groupCards :: [Card] -> [[Card]]
groupCards = foldr addToGroup [] 
  where addToGroup :: Card -> [[Card]] -> [[Card]]
        addToGroup c [] = [[c]]
        addToGroup c (a:as) = if head a == c then (c:a) : as else a : addToGroup c as


compareHands :: [Card] -> (HandType, [Card]) -> (HandType, [Card]) -> Ordering
compareHands cardsOrdered (ta, ha) (tb, hb) = case compare ta tb of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> compareSameTypeHands cardsOrdered ha hb


compareSameTypeHands :: [Card] -> [Card] -> [Card] -> Ordering
compareSameTypeHands _ _ [] = EQ
compareSameTypeHands _ [] _ = EQ
compareSameTypeHands cardsOrdered (a:as) (b:bs) = case compareCards cardsOrdered a b of
                                      EQ -> compareSameTypeHands cardsOrdered as bs
                                      other -> other

compareCards :: [Card] -> Card -> Card -> Ordering
compareCards cardsOrdered a b = compareCards' a b cardsOrdered
  where compareCards' a b [] = error $ "Illegal state: one of the cards is unknown: " ++ [a] ++ " or " ++ [b]
        compareCards' a b (c:cs)
          | a == b = EQ
          | a == c = LT
          | b == c = GT
          | otherwise = compareCards' a b cs
