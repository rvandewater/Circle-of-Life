module Main where

import Controller
import Model
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do   level_1 <- loadBMP "level_1.bmp"
            level_2 <- loadBMP "level_2.bmp"
            level_3 <- loadBMP "level_3.bmp"                                    -- Different level backgrounds
            rg <- getStdGen                                                     -- Generator
            playIO (InWindow "Circle of Life" (screenx, screeny) (500, 0))      -- Or FullScreen
                    (makeColor 0.5 0.5 0.5 1)                                   -- Background color
                    60                                                          -- Frames per second
                    (initialState rg)                                           -- Initial state
                    (view [level_1, level_2, level_3])                          -- View function
                    input                                                       -- Event function
                    step                                                        -- Step function
