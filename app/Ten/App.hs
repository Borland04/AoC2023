{-# LANGUAGE BangPatterns #-}

module Ten.App where

import Data.List(find, findIndex)
import Control.Applicative(asum)
import Control.Monad(sequence_)
import Data.Maybe(isJust, fromJust)
import Data.Map.Strict((!?))
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy(State, get, put, execState)

--import Text.Printf(printf)

data Pipe = Horizontal
          | Vertical
          | NW
          | NE
          | SE
          | SW
          | Start
          | None
          deriving (Show, Eq)

pipeChars :: [(Char, Pipe)] 
pipeChars = [('-', Horizontal), ('|', Vertical), ('L', NE), ('J', NW), ('7', SW), ('F', SE), ('S', Start)]

findAnimal :: IO ()
findAnimal = do
  content <- lines <$> readFile "app/Ten/input.txt"
  let islandMap = parseInput content
  let res = foldr max 0 . M.elems $ traversePipes islandMap
  -- let shw = showMap islandMap (traversePipes islandMap) (length . show $ res)
  -- writeFile "app/Ten/output.txt" shw
  putStrLn $ "# Task 0: " ++ show res

-- showMap :: [[Pipe]] -> M.Map (Int, Int) Int -> Int -> String
-- showMap pipes steps padding = unlines $ map showPipeLine pipes ++ ["", showSteps 0 0]
--   where showPipeLine = map (\p -> maybe '.' fst $ find ((== p) . snd) pipeChars)
--         showSteps r c
--           | r == length pipes && c == length (pipes !! 0) = ""
--           | c == length (pipes !! 0) = "\n" ++ showSteps (r + 1) 0
--           | otherwise = let cell = (maybe (take (padding+1) ("." ++ repeat ' ')) (printf ("%" ++ show padding ++ "d ")) (steps !? (r, c)))
--                         in cell ++ (showSteps r (c+1))

parseInput :: [String] -> [[Pipe]]
parseInput = map parseLine
  where parseLine = map parseChar
        parseChar c = maybe None id (snd <$> find ((== c) . fst) pipeChars)

traversePipes :: [[Pipe]] -> M.Map (Int, Int) Int
traversePipes pipes = let startRow = maybe (error "Illegal state: start point not found") id $ findIndex (any (== Start)) pipes
                          startCol = maybe (error "Illegal state: start point not found") id $ findIndex (== Start) (pipes !! startRow)
    
                in execState (step 0 startRow startCol) M.empty
  where step :: Int -> Int -> Int -> State (M.Map (Int, Int) Int) ()
        step stepNum r c = do
          m <- get
          let maybeSavedSteps = m !? (r, c)
          if (isJust maybeSavedSteps && stepNum >= fromJust maybeSavedSteps)
            then return ()
            else put (M.insert (r, c) stepNum m) >> do
                   let nxt = nextSteps r c (pipes !! r !! c)
                   let safeNxt = filter isInBorders $ nxt
                   let !availNxt = filter (isAvailableToMove (r, c)) safeNxt 
                   sequence_ $ map (\(r', c') -> step (stepNum + 1) r' c') availNxt

        nextSteps :: Int -> Int -> Pipe -> [(Int, Int)]
        nextSteps r c pipe
          | pipe == Horizontal = [(r, c - 1), (r, c + 1)]
          | pipe == Vertical = [(r - 1, c), (r + 1, c)]
          | pipe == NW = [(r - 1, c), (r, c - 1)]
          | pipe == NE = [(r - 1, c), (r, c + 1)]
          | pipe == SW = [(r + 1, c), (r, c - 1)]
          | pipe == SE = [(r + 1, c), (r, c + 1)]
          | pipe == Start = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]
          | otherwise = []

        isInBorders (r', c') = r' >= 0 && r' < length pipes && c' >= 0 && c' < length (pipes !! 0)

        isAvailableToMove :: (Int, Int) -> (Int, Int) -> Bool
        isAvailableToMove from to@(tr, tc) = let nextPipe = pipes !! tr !! tc
                                             in any (== nextPipe) $ availableToMove from to
          where availableToMove (fr, fc) (tr, tc)
                  | fr - 1 == tr = [Vertical, SW, SE] -- Move North
                  | fr + 1 == tr = [Vertical, NW, NE] -- Move South
                  | fc - 1 == tc = [Horizontal, NE, SE] -- Move West
                  | fc + 1 == tc = [Horizontal, NW, SW] -- Move East
                  | otherwise = []
