module One.App where

import Data.Char (isDigit, toLower)
import Data.List (find, isPrefixOf)
import Control.Applicative((<|>))

stringifiedDigitsMap :: [(String, String)]
stringifiedDigitsMap = [
    ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9"),
    ("zero", "0")]


readCalibrations :: String -> IO ()
readCalibrations filename = do
  content <- lines <$> readFile filename
  let res = foldr (+) 0 . map readCalibrationLine $ content
  let res2 = foldr (+) 0 . map readCalibrationLine2 $ content
  putStrLn $ "Task 1: " ++ show res
  putStrLn $ "Task 2: " ++ show res2


-- Task 1
readCalibrationLine :: String -> Int
readCalibrationLine s = (read (fstDigit s) :: Int) * 10 + (read (lstDigit s) :: Int)
  where fstDigit :: String -> String
        fstDigit s = let s' = dropWhile (not . isDigit) s
                     in case s' of
                       [] -> error $ "No digits in input string: " ++ s
                       _ -> (head s') : []
        lstDigit :: String -> String
        lstDigit = fstDigit . reverse


-- Task 2
readCalibrationLine2 :: String -> Int
readCalibrationLine2 s = (read (fstDigit2 s) :: Int) * 10 + (read (lstDigit2 s) :: Int)
        where readHeadDigit :: String -> Maybe String
              readHeadDigit [] = Nothing
              readHeadDigit s
                | isDigit (head s) = Just $ head s : [] 
                | otherwise =  fmap snd . find (flip isPrefixOf (map toLower s) . fst) $ stringifiedDigitsMap

              fstDigit2 :: String -> String
              fstDigit2 [] = error $ "No digits in input string: " ++ s
              fstDigit2 s = maybe (fstDigit2 . tail $ s) id (readHeadDigit s)

              lstDigit2 :: String -> String
              lstDigit2 s = maybe (error $ "No digits in input string: " ++ s) id $ lstDigit2' s
              lstDigit2' [] = Nothing
              lstDigit2' s = lstDigit2' (tail s) <|> readHeadDigit s
