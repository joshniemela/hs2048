module Main where

import Data.List
import Main.Utf8 qualified as Utf8
import System.IO (getChar, putStr)
import System.Random (randomRIO)
import Prelude hiding (Down, Left, Right, putStr)

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

-- REFACTOR THE BELOW TO BE ONE FUNCTION
randomValue :: IO Int
randomValue = randomRIO (1, 10)

twoOrFour :: Int -> Int
twoOrFour 1 = 4
twoOrFour _ = 2

chooseRandom :: [a] -> IO a
chooseRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

-- Random where 2 has a 90% chance of being chosen and 4 has a 10% chance
newTileValue :: IO Int
newTileValue = do twoOrFour <$> randomValue

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
  | n == m = Just (n + m) : row
  | otherwise = Just n : Just m : row
mergeTile (Just n) (Nothing : row) = Just n : row

-- Move all tiles in a row to the right and place a new tile
move :: Move -> Grid -> IO Grid
move move grid = do
  let newGrid = case move of
        Up -> transpose . map (reverse . mergeRowRight . reverse) . transpose $ grid
        Down -> transpose . map mergeRowRight . transpose $ grid
        Right -> map mergeRowRight grid
        Left -> map (reverse . mergeRowRight . reverse) grid
  if newGrid == grid
    then return grid
    else placeRandomTile newGrid

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
    & putStrLn

-- Display a single tile
displayTile :: Tile -> String
displayTile Nothing = "    "
displayTile (Just n) = show n ++ "   " & take 4

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"
loop :: Grid -> IO ()
loop grid = do
  putStrLn "\n"
  putStrLn "--------------------------"
  displayGrid grid
  input <- getChar
  clearScreen

  case input of
    'w' -> loop =<< move Up grid
    's' -> loop =<< move Down grid
    'a' -> loop =<< move Left grid
    'd' -> loop =<< move Right grid
    _ -> loop grid

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  print "Welcome to 2048! Press any key to start."
  let grid = emptyGrid
  grid' <- placeRandomTile grid >>= placeRandomTile

  loop grid'