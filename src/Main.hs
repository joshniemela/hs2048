module Main where

import Control.Concurrent.Async
import Data.ByteString qualified as BS
import Network.Simple.TCP
import System.IO (getChar, putStr)
import Prelude hiding (Down, Left, Right, putStr, sum)

import GameLogic
import Render

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
