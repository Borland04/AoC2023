module Eight.App where

import Text.Parsec(parse, skipMany, space, many1, alphaNum, char)
import Text.Parsec.String(Parser)
import Data.Map((!))
import qualified Data.Map as Map
import Debug.Trace(traceShow)

data Direction = LeftDir | RightDir deriving Show
type Location = String
data MapNode = MapNode { from :: Location, leftTo :: Location, rightTo :: Location } deriving Show

navigateDesert :: IO ()
navigateDesert = do
  content <- lines <$> readFile "app/Eight/input.txt"
  let (dirs, desertMap) = parseInput content
  let res = traversePath desertMap (== "AAA") (== "ZZZ") dirs
  putStrLn $ "# Task 1: " ++ show res
  let res2 = traversePath desertMap ((== 'A') . last) ((== 'Z') . last) dirs
  putStrLn $ "# Task 2: " ++ show res2

traversePath :: Map.Map Location MapNode -> (Location -> Bool) -> (Location -> Bool) -> [Direction] -> Int
traversePath desertMap startPredicate endPredicate allDirs = traversePath' desertMap (map (\loc -> (desertMap ! loc, False, [])) . filter startPredicate $ (Map.keys desertMap)) allDirs 0
  where traversePath' :: Map.Map Location MapNode -> [(MapNode, Bool, [Int])] -> [Direction] -> Int -> Int
        traversePath' m c [] acc = traversePath' m c allDirs acc
        traversePath' m currLocs (d:ds) acc 
          | all (\(node, finished, cachedEnds) -> (not finished && endPredicate (from node)) || finished && any (\e -> acc `mod` e == 0) cachedEnds) currLocs = acc
          | otherwise= let nextLocs = map handleNode currLocs
                       in acc `seq` traversePath' m nextLocs ds (acc + 1)
          where handleNode :: (MapNode, Bool, [Int]) -> (MapNode, Bool, [Int])
                handleNode mapNode@(node, finished, cachedEnds)
                  | finished = mapNode
                  | endPredicate (from node) = if (not . null) cachedEnds && (acc `mod` head cachedEnds) == 0 then (node, True, cachedEnds) else (nextLoc node, False, cachedEnds ++ [acc])
                  | otherwise = (nextLoc node, False, cachedEnds)
                nextLoc = (m !) . nextLocSupplier
                nextLocSupplier = case d of LeftDir -> leftTo; RightDir -> rightTo;

parseInput :: [String] -> ([Direction], Map.Map Location MapNode)
parseInput (dirsStr:_:nodeStrs) = (parseDirections dirsStr, foldr (\node acc -> Map.insert (from node) node acc) Map.empty . map parseNode $ nodeStrs)
parseInput _ = error "Illegal state: expected at least 3 lines of input"

parseDirections :: String -> [Direction]
parseDirections = map mapDirection
  where mapDirection c = case c of
                          'L' -> LeftDir
                          'R' -> RightDir
                          _   -> error $ "Illegal state: invalid char in directions line: " ++ [c]

parseNode :: String -> MapNode
parseNode s = either (\e -> error $ "Failed to parse map node: " ++ show e) id $ parse nodeParser "Node Parser" s
  where nodeParser :: Parser MapNode
        nodeParser = do
          source <- node <* skipSpaces
          _ <- char '=' <* skipSpaces
          _ <- char '(' <* skipSpaces
          l <- node <* skipSpaces
          _ <- char ',' <* skipSpaces
          r <- node <* skipSpaces
          _ <- char ')' <* skipSpaces
          return $ MapNode source l r

        node = many1 alphaNum
        skipSpaces = skipMany space
