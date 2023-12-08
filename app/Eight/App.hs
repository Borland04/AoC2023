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
traversePath desertMap startPredicate endPredicate allDirs =  foldr min maxBound . map (foldr lcm 1) . foldr (\xs acc -> [x : as | x <- xs, as <- acc]) [[]] . map (safeTraverse 0 Map.empty allDirs) . map (desertMap !) . filter startPredicate $ (Map.keys desertMap)
  where safeTraverse :: Int -> Map.Map Location [Int] -> [Direction] -> MapNode -> [Int]
        safeTraverse step traverseLog [] n = safeTraverse step traverseLog allDirs n
        safeTraverse step traverseLog (d:ds) n = step `seq` if (endPredicate . from $ n)
                                                              then (if any (== (length ds)) (Map.findWithDefault [] (from n) traverseLog)
                                                                then []
                                                                else step : safeTraverse (step + 1) (Map.insertWith (++) (from n) [length ds] traverseLog) ds (nextNode d n))
                                                              else safeTraverse (step + 1) traverseLog ds (nextNode d n)
        nextNode d n = desertMap ! nextStepSupplier d n
        nextStepSupplier LeftDir = leftTo
        nextStepSupplier RightDir = rightTo


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
