module Ten.App where

import Data.List(find, findIndex)
import Control.Applicative(asum)
import Control.Monad(sequence_)
import Data.Maybe(isJust, fromJust)
import Data.Map.Strict((!?), (!))
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Lazy(State, get, put, execState)

import Debug.Trace(trace)

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
  let pipes = islandMap `seq` traversePipes islandMap
  let res = foldr max 0 . M.elems $ pipes
  putStrLn $ "# Task 1: " ++ show res
  --let areas = M.empty
  let areas = traverseEnclosedArea pipes islandMap
  let res2 = length . M.keys $ areas
  putStrLn $ "# Task 2: " ++ show res2
  writeFile "app/Ten/output.txt" $ debugMap islandMap pipes areas

parseInput :: [String] -> [[Pipe]]
parseInput = map parseLine
  where parseLine = map parseChar
        parseChar c = maybe None id (snd <$> find ((== c) . fst) pipeChars)

debugMap :: [[Pipe]] -> M.Map (Int, Int) Int -> M.Map (Int, Int) Char -> String
debugMap pipes pipesMap areaMap = unlines $ map showLine [0..(length pipes - 1)]
  where showLine r = map (\c -> if M.member (r, c) areaMap then 'I' else if M.member (r, c) pipesMap then (pipeSymbol (r, c)) else '.') [0..(length (pipes !! r) - 1)]
        --pipeSymbol (r', c') = maybe '.' fst $ find ((== pipes !! r' !! c') . snd) pipeChars
        pipeSymbol p = last . show $ pipesMap ! p

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
                   let safeNxt = filter (isInBorders pipes) $ nxt
                   let availNxt = filter (isAvailableToMove pipes (r, c)) safeNxt 
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


isInBorders :: [[Pipe]] -> (Int, Int) -> Bool
isInBorders pipes (r', c') = r' >= 0 && r' < length pipes && c' >= 0 && c' < length (pipes !! 0)

isAvailableToMove :: [[Pipe]] -> (Int, Int) -> (Int, Int) -> Bool
isAvailableToMove pipes from to@(tr, tc) = let nextPipe = pipes !! tr !! tc
                                     in any (== nextPipe) $ availableToMove from to
  where availableToMove (fr, fc) (tr, tc)
          | fr - 1 == tr = [Vertical, SW, SE, Start] -- Move North
          | fr + 1 == tr = [Vertical, NW, NE, Start] -- Move South
          | fc - 1 == tc = [Horizontal, NE, SE, Start] -- Move West
          | fc + 1 == tc = [Horizontal, NW, SW, Start] -- Move East
          | otherwise = []

-- Task 2

traverseEnclosedArea :: M.Map (Int, Int) Int -> [[Pipe]] -> M.Map (Int, Int) Char
traverseEnclosedArea pipeMap pipes = let topLeftPipe = head . filter (flip M.member pipeMap) $ [(r, c) | r <- [0..(length pipes - 1)], c <- [0..(length (pipes !! 0) - 1)]]
                                     in traversePipeLoop pipeMap pipes topLeftPipe

traversePipeLoop :: M.Map (Int, Int) Int -> [[Pipe]] -> (Int, Int) -> M.Map (Int, Int) Char
traversePipeLoop pipeMap pipes topLeftPipe@(tlr, tlc) = execState (touchClockwise topLeftPipe (tlr, tlc + 1)) M.empty -- Always will be SE (or Start) pipe
  where touchClockwise :: (Int, Int) -> (Int, Int) -> State (M.Map (Int, Int) Char) ()
        touchClockwise from cur 
          | cur == topLeftPipe = return ()
          | otherwise= do
                        sequence_ $ map markEnclosedArea (enclosingCell from cur)
                        let clockwiseRotation = clockwiseNeighbors cur
                        let nxt = head . filter (\n -> abs (pipeMap ! cur - pipeMap ! n) == 1) . filter (isAvailableToMove pipes cur) . filter (/= from) . filter (isInBorders pipes) . filter (flip M.member pipeMap) $ clockwiseRotation
                        touchClockwise cur nxt

        markEnclosedArea :: (Int, Int) -> State (M.Map (Int, Int) Char) ()
        markEnclosedArea (r, c)
          | not (isInBorders pipes (r, c)) = error "Mark is out of border"
          | M.member (r, c) pipeMap = return ()
          | otherwise = do
            areaMap <- get
            if M.member (r, c) areaMap
              then return ()
              else do
                put (M.insert (r, c) 'I' areaMap)
                let neighbors = filter (not . flip M.member pipeMap) $ clockwiseNeighbors (r, c)
                sequence_ $ map markEnclosedArea neighbors
        
        enclosingCell :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
        enclosingCell (fr, fc) (cr, cc)
          | fr == cr - 1 = case pipes !! cr !!cc of
                            NW -> [(cr-1, cc - 1)]
                            NE -> [(cr, cc - 1), (cr + 1, cc)]
                            _ -> [(cr, cc - 1)]
          | fr == cr + 1 = case pipes !! cr !! cc of
                            SW -> [(cr, cc + 1), (cr - 1, cc)]
                            SE -> [(cr + 1, cc + 1)]
                            _ -> [(cr, cc + 1)]
          | fc == cc - 1 = case pipes !! cr !! cc of
                            NW -> [(cr + 1, cc), (cr, cc + 1)]
                            SW -> [(cr + 1, cc - 1)]
                            _ -> [(cr + 1, cc)]
          | fc == cc + 1 = case pipes !! cr !! cc of
                            NE -> [(cr - 1, cc + 1)]
                            SE -> [(cr - 1, cc), (cr, cc - 1)]
                            _ -> [(cr - 1, cc)]
          | otherwise = error "Not possible: 'from position' is not a neighbor for current position"
        
        clockwiseNeighbors :: (Int, Int) -> [(Int, Int)]
        clockwiseNeighbors (r, c) = [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
