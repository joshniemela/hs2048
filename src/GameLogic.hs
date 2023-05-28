module GameLogic where

import Data.List
import System.IO (getChar, putStr)
import Prelude hiding (Down, Left, Right, putStr, sum)

import System.Random

-- The game is comprised of a 4x4 grid, each tile either no value or an integer,
-- Normally 2048 will use powers of two, to calculate the score heuristically,
-- I save them in log2
type Tile = Maybe Int
type Grid = [[Tile]]
data Move = Up | Down | Left | Right

emptyGrid :: Grid
emptyGrid = replicate 4 (replicate 4 Nothing)

-- Find all empty tiles in the grid
findEmptyTiles :: Grid -> [(Int, Int)]
findEmptyTiles grid = do
    (row, rowTiles) <- zip [0 ..] grid
    (column, tile) <- zip [0 ..] rowTiles
    case tile of
        Nothing -> [(row, column)]
        Just _ -> []

-- Insert tile into the grid given coordinates
placeTile :: Tile -> (Int, Int) -> Grid -> Grid
placeTile tile (rowIdx, colIdx) grid = beforeTileRow ++ [tileRow] ++ afterTileRow
  where
    beforeTileRow = take rowIdx grid
    tileRow = beforeTileColumn ++ [tile] ++ afterTileColumn
    afterTileRow = drop (rowIdx + 1) grid
    beforeTileColumn = take colIdx (grid !! rowIdx)
    afterTileColumn = drop (colIdx + 1) (grid !! rowIdx)

placeRandomTile :: Grid -> IO Grid
placeRandomTile grid =
    findEmptyTiles grid
        & chooseRandom
        >>= \tile -> newTileValue >>= \value -> return (placeTile (Just value) tile grid)
  where
    -- Generate a random tile with the value 1 or 2 with a 90%, 10% distribution (using log2)
    newTileValue :: IO Int
    newTileValue = (randomRIO (1, 10) :: IO Int) >>= \n -> return (if n == 1 then 2 else 1)

    -- Select a random tile in the list
    chooseRandom :: [a] -> IO a
    chooseRandom xs = randomRIO (0, length xs - 1) <&> (xs !!)

-- Move and merge all tiles in a row to the left
mergeRowLeft :: [Tile] -> [Tile]
mergeRowLeft [] = []
mergeRowLeft (Nothing : xs) = mergeRowLeft xs ++ [Nothing]
mergeRowLeft (Just x : Nothing : xs) = mergeRowLeft (Just x : xs) ++ [Nothing]
mergeRowLeft (Just x : Just y : xs)
    | x == y = Just (x + 1) : mergeRowLeft xs ++ [Nothing]
    | otherwise = Just x : mergeRowLeft (Just y : xs)
mergeRowLeft (Just x : xs) = Just x : mergeRowLeft xs

-- Move all tiles in a direction
moveBoard :: Move -> Grid -> Grid
moveBoard Left = map mergeRowLeft
moveBoard Right = map (reverse . mergeRowLeft . reverse)
moveBoard Up = transpose . moveBoard Left . transpose
moveBoard Down = transpose . moveBoard Right . transpose

checkGameOver :: Grid -> IO (Maybe Grid)
checkGameOver grid =
    if null (findEmptyTiles grid) && not (canMerge grid)
        then -- \r prevents last keystroke being visible
        -- \n prevents the cursor from sitting ontop of the game after exitting
            putStr "\rGame over!\n" >> return Nothing
        else return (Just grid)
  where
    -- Try moving in all four directions, if all moves are the same, then the game is over since
    -- there are no legal moves left
    canMerge :: Grid -> Bool
    canMerge grid = any (/= grid) (map moveBoard [Up, Down, Left, Right] <*> [grid])

move :: Move -> Grid -> IO (Maybe Grid)
move moveOp grid = do
    let newGrid = moveBoard moveOp grid
    if newGrid /= grid
        then placeRandomTile newGrid >>= checkGameOver
        else return (Just grid)

legalMoves :: Grid -> [Move]
legalMoves grid = [Up, Down, Left, Right] & filter (canMove grid)
  where
    canMove :: Grid -> Move -> Bool
    canMove grid moveOp = moveBoard moveOp grid /= grid
