module Main where

import Data.List
import System.IO (getChar, putStr)
import System.Random
import Prelude hiding (Down, Left, Right, putStr, sum)

{- |
 Main entry point.

 The `bin/run` script will invoke this function.
-}
type Tile = Maybe Int

type Grid = [[Tile]]

data Move = Up | Down | Left | Right

emptyGrid :: Grid
emptyGrid = replicate 4 (replicate 4 Nothing)

-- Insert tile into the grid given coordinates
placeTile :: Tile -> (Int, Int) -> Grid -> Grid
placeTile tile (row, column) grid = beforeTileRow ++ [tileRow] ++ afterTileRow
  where
    beforeTileRow = take row grid
    tileRow = beforeTileColumn ++ [tile] ++ afterTileColumn
    afterTileRow = drop (row + 1) grid
    beforeTileColumn = take column (grid !! row)
    afterTileColumn = drop (column + 1) (grid !! row)

-- Find all empty tiles in the grid
findEmptyTiles :: Grid -> [(Int, Int)]
findEmptyTiles grid = do
  (row, rowTiles) <- zip [0 ..] grid
  (column, tile) <- zip [0 ..] rowTiles
  case tile of
    Nothing -> [(row, column)]
    Just _ -> []

-- Generate a random tile value between 2 or 4 with a 90%, 10% weight
newTileValue :: IO Int
newTileValue = (randomRIO (1, 10) :: IO Int) >>= \n -> return (if n == 1 then 2 else 1)

chooseRandom :: [a] -> IO a
chooseRandom xs = randomRIO (0, length xs - 1) <&> (xs !!)

placeRandomTile :: Grid -> IO Grid
placeRandomTile grid = do
  let emptyTiles = findEmptyTiles grid
  position <- chooseRandom emptyTiles
  value <- newTileValue
  return $ placeTile (Just value) position grid

-- Move all tiles in a row to the right
mergeRowRight :: [Tile] -> [Tile]
mergeRowRight row = emptyRow ++ mergedRow
  where
    mergedRow = foldr mergeTile [] row
    emptyRow = replicate (4 - length mergedRow) Nothing

-- Merge two tiles if they are equal
mergeTile :: Tile -> [Tile] -> [Tile]
mergeTile Nothing row = row
mergeTile (Just n) [] = [Just n]
mergeTile (Just n) (Just m : row)
  | n == m = Just (n + 1) : row
  | otherwise = Just n : Just m : row
mergeTile (Just n) (Nothing : row) = Just n : row

checkGameOver :: Grid -> IO (Maybe Grid)
checkGameOver grid =
  if null (findEmptyTiles grid) && not (canMerge grid)
    then putStr "Game over!" >> return Nothing
    else return (Just grid)

-- Try each merge and see if any of the grids are different to the original grid
canMerge :: Grid -> Bool
canMerge grid =
  any
    (grid /=)
    [ map mergeRowRight grid
    , map (reverse . mergeRowRight . reverse) grid
    , transpose . map mergeRowRight . transpose $ grid
    , transpose . map (reverse . mergeRowRight . reverse) . transpose $ grid
    ]

-- refactor and rename
recurrentScore :: Maybe Int -> Int
recurrentScore Nothing = 0
-- The additional term is to account for the randomly spawned 4s
recurrentScore (Just n) = 2 ^ n * (n - 1) - quot (2 ^ (n + 1)) 11

-- map reccurrentScore to each element of the grid and sum the result
scoreGrid :: Grid -> Int
scoreGrid grid = sum $ map (sum . map recurrentScore) grid

move :: Move -> Grid -> IO (Maybe Grid)
move moveOp grid = do
  let newGrid = case moveOp of
        Up -> transpose . map (reverse . mergeRowRight . reverse) . transpose $ grid
        Down -> transpose . map mergeRowRight . transpose $ grid
        Right -> map mergeRowRight grid
        Left -> map (reverse . mergeRowRight . reverse) grid
  if newGrid /= grid
    then placeRandomTile newGrid >>= checkGameOver
    else return (Just grid)

-- Display the 4x4 grid in the terminal
displayGrid :: Grid -> IO ()
displayGrid grid =
  "┌────┬────┬────┬────┐\n"
    ++ "│"
    ++ intercalate "│" (map displayTile (grid !! 0))
    ++ "│\n"
    ++ "├────┼────┼────┼────┤\n"
    ++ "│"
    ++ intercalate "│" (map displayTile (grid !! 1))
    ++ "│\n"
    ++ "├────┼────┼────┼────┤\n"
    ++ "│"
    ++ intercalate "│" (map displayTile (grid !! 2))
    ++ "│\n"
    ++ "├────┼────┼────┼────┤\n"
    ++ "│"
    ++ intercalate "│" (map displayTile (grid !! 3))
    ++ "│\n"
    ++ "└────┴────┴────┴────┘\n"
    ++ "Score: "
    ++ show (scoreGrid grid)
    & putStrLn

-- Display a single tile
displayTile :: Tile -> String
displayTile Nothing = "    "
-- Convert log 2 of a number to number, this is needed since the game uses logarithms
displayTile (Just n) = show (2 ^ n) ++ "   " & take 4

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

loop :: Maybe Grid -> IO ()
loop Nothing = pass
loop (Just grid) = do
  clearScreen
  putStrLn "\n"
  putStrLn "--------------------------"
  displayGrid grid
  input <- getChar

  case input of
    'w' -> loop =<< move Up grid
    's' -> loop =<< move Down grid
    'a' -> loop =<< move Left grid
    'd' -> loop =<< move Right grid
    'q' -> putStrLn "Bye!"
    _ -> loop (Just grid)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  print ("Welcome to 2048! Press any key to start." :: String)
  grid' <- placeRandomTile emptyGrid >>= placeRandomTile

  loop (Just grid')