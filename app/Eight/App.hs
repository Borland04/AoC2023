module Eight.App where

import Text.Parsec(parse, skipMany, space, many1, letter, char)
import Text.Parsec.String(Parser)
import Data.Map((!))
import qualified Data.Map as Map

data Direction = LeftDir | RightDir deriving Show
type Location = String
data MapNode = MapNode { from :: Location, leftTo :: Location, rightTo :: Location } deriving Show

navigateDesert :: IO ()
navigateDesert = do
  content <- lines <$> readFile "app/Eight/input.txt"
  let (dirs, desertMap) = parseInput content
  let res = traversePath desertMap "AAA" dirs
  putStrLn $ "# Task 1: " ++ show res

traversePath :: Map.Map Location MapNode -> Location -> [Direction] -> Int
traversePath desertMap start allDirs = traversePath' desertMap (desertMap ! start) allDirs 0
  where traversePath' :: Map.Map Location MapNode -> MapNode -> [Direction] -> Int -> Int
        traversePath' _ (MapNode "ZZZ" _ _) _ acc = acc
        traversePath' m c [] acc = traversePath' m c allDirs acc
        traversePath' m currLoc (d:ds) acc = let nextLocationSupplier = case d of LeftDir -> leftTo; RightDir -> rightTo;
                                             in acc `seq` traversePath' m (m ! (nextLocationSupplier currLoc)) ds (acc + 1)


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

        node = many1 letter
        skipSpaces = skipMany space
