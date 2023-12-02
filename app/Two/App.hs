{-# LANGUAGE OverloadedStrings #-}
module Two.App where

import Data.Char(isDigit)
import Data.Text(Text, splitOn, pack, unpack, strip)
import Data.List(find)

type RevealedCube = (String, Int)
type CubeSet = [RevealedCube]
data Game = Game { gameId :: Int, sets :: [CubeSet] } deriving Show

playCubesGame :: CubeSet -> String -> IO ()
playCubesGame constraints filename = do
  content <- lines <$> readFile filename
  let games = map parseGame content
  let res = foldr (+) 0 . map gameId . filter (isGamePossible constraints) $ games
  putStrLn $ "Task 1: " ++ show res

parseGame :: String -> Game
parseGame s = readSets . dropColon . readGameId . dropGame $ (s, Game)
  where dropGame :: (String, a) -> (String, a)
        dropGame (s, f) = (drop (length ("Game " :: String)) s, f)

        readGameId :: (String, Int -> a) -> (String, a)
        readGameId (s, f) = let (idStr, ss) = span isDigit s
                            in (ss, f $ read idStr)

        dropColon :: (String, a) -> (String, a) 
        dropColon (s, f) = (drop 1 s, f)

        readSets :: (String, [CubeSet] -> a) -> a
        readSets (s, f) = f . map readSet $ splitOn ";" (pack s)

        readSet :: Text -> CubeSet
        readSet s = map readCube . splitOn "," $ s


        readCube :: Text -> RevealedCube
        readCube s = let [num, color] = splitOn " " $ strip s
                     in (unpack color, read (unpack num) :: Int)

isGamePossible :: CubeSet -> Game -> Bool
isGamePossible constraints (Game _ cubesets) = all (\set -> all (notViolatesConstraint constraints) set) cubesets
  where notViolatesConstraint :: CubeSet -> RevealedCube -> Bool
        notViolatesConstraint constr (color, num) = case find ((== color) . fst) constr of
                                                      Just (_, maxNum) -> num <= maxNum
                                                      Nothing -> False
