module Render where

import Data.List
import System.IO (getChar, putStr)
import Prelude hiding (Down, Left, Right, putStr, sum)

import GameLogic (Grid, Tile)

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
