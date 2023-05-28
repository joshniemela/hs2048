module Main where

import Control.Concurrent.Async
import Data.ByteString qualified as BS
import Data.List
import Network.Simple.TCP
import System.IO (getChar, putStr)
import System.Random

import Prelude hiding (Down, Left, Right, putStr, sum)

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

        -- \r prevents last keystroke being visible
        -- \n prevents the cursor from sitting ontop of the game after exitting
        then putStr "\rGame over!\n" >> return Nothing
        else return (Just grid)
  where
    -- Try moving in all four directions, if all moves are the same, then the game is over since
    -- there are no legal moves left
    canMerge :: Grid -> Bool
    canMerge grid = any (/= grid) (map moveBoard [Up, Down, Left, Right] <*> [grid])

-- This function calculates the approximate score of the game using the recurrence relation:
-- a_n = 2a_{n-1}+2^n
-- This is solved for the full equation and the randomly spawned 4s are acconted for with
-- the last term of recurrentScore
recurrentScore :: Maybe Int -> Int
recurrentScore Nothing = 0
-- The additional term is to account for the randomly spawned 4s
recurrentScore (Just n) = 2 ^ n * (n - 1) - quot (2 ^ (n + 1)) 11

-- map recurrentScore to each element of the grid and sum the result
scoreGrid :: Grid -> Int
scoreGrid grid = sum $ map (sum . map recurrentScore) grid

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
  where
    -- Display a single tile
    displayTile :: Tile -> String
    displayTile Nothing = "    "
    -- Convert the log2 representation of a tile to normal linear space
    displayTile (Just n) = show (2 ^ n :: Int) ++ "   " & take 4

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J" -- ANSI code for wiping the entire screen

-- This functions is responsible for the game loop, it listens for an input on the keyboard,
-- updates the state, and repeats until the game is over
loop :: Maybe Grid -> IO ()
loop Nothing = pass
loop (Just grid) = do
    clearScreen
    putStrLn "\r" 
    displayGrid grid
    input <- getChar

    case input of
        'w' -> loop =<< move Up grid
        's' -> loop =<< move Down grid
        'a' -> loop =<< move Left grid
        'd' -> loop =<< move Right grid
        -- \r prevents last keystroke being visible
        -- \n prevents the cursor from sitting ontop of the game after exitting
        'q' -> putStrLn "\rBye!\n"
        _ -> loop (Just grid)

-- Take grid which is a list of lists of tiles and convert it to a string
encode :: Maybe Grid -> ByteString
encode Nothing = "gameover"
-- get the score of the grid and encode it as a string using recurrent score
encode (Just grid) =
    show (scoreGrid grid)
        <> " "
        <> BS.intercalate " " (map encodeTile (concat grid))
        <> " "
        <> BS.intercalate " " (map encodeMove (legalMoves grid))
        <> "\n"
  where
    encodeTile :: Tile -> ByteString
    encodeTile Nothing = "0"
    encodeTile (Just n) = show n

    encodeMove :: Move -> ByteString
    encodeMove Up = "1"
    encodeMove Left = "2"
    encodeMove Down = "3"
    encodeMove Right = "4"

-- Variation of the loop function which is also able to connect via TCP and listen for inputs
-- with TCP instead of only just the keyboard
socketLoop :: Maybe Grid -> Socket -> IO ()
socketLoop Nothing _ = pass
socketLoop (Just grid) socket = do
    clearScreen
    putStrLn "\n"
    displayGrid grid

    -- Get head or bytestring or get keyboard input
    input <- race getChar (recv socket 1)

    input
        & either
            ( \case
                -- Keyboard input
                'w' -> move Up grid
                'a' -> move Left grid
                's' -> move Down grid
                'd' -> move Right grid
                'q' -> putStrLn "Bye!" >> return Nothing
                _ -> return (Just grid)
            )
            ( \case
                -- TCP input
                Just "w" -> move Up grid
                Just "a" -> move Left grid
                Just "s" -> move Down grid
                Just "d" -> move Right grid
                Just "q" -> putStrLn "Bye!" >> return Nothing
                _ -> return (Just grid)
            )
        -- Write new grid and return state on socket and convert grid to bytestring
        >>= \newGrid -> send socket (encode newGrid) >> socketLoop newGrid socket

-- Read port command line argument, if not present start normal loop
main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdin NoBuffering
    initialGrid <- placeRandomTile emptyGrid >>= placeRandomTile
    case args of
        ["-p", port] -> do
            connect "localhost" port $ \(socket, _) -> do
                send socket (encode (Just initialGrid))
                socketLoop (Just initialGrid) socket
        ["-h"] -> putStrLn "Options:\n -p <port> for game over network\n -h for help\n No arguments for local game"
        ["-v"] -> putStrLn "Version 1.1.1" >> exitSuccess
        _ -> loop (Just initialGrid)
