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
  let res2 = foldr (+) 0 . map power $ games
  putStrLn $ "Task 2: " ++ show res2

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

-- Task 1
isGamePossible :: CubeSet -> Game -> Bool
isGamePossible constraints (Game _ cubesets) = all (\set -> all (notViolatesConstraint constraints) set) cubesets
  where notViolatesConstraint :: CubeSet -> RevealedCube -> Bool
        notViolatesConstraint constr (color, num) = case find ((== color) . fst) constr of
                                                      Just (_, maxNum) -> num <= maxNum
                                                      Nothing -> False

-- Task 2
power :: Game -> Int
power (Game _ cubeset) = foldr (*) 1 . map snd . foldr accumulateCubes [] $ concat cubeset
  where accumulateCubes :: RevealedCube -> CubeSet -> CubeSet
        accumulateCubes c [] = [c]
        accumulateCubes c@(color, num) (acc:accs)
          | fst acc == color = (color, max num (snd acc)) : accs
          | otherwise = acc : accumulateCubes c accs
