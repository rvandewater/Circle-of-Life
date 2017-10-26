module Main where
    
    import Controller
    import Model
    import View
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    
    main :: IO ()
    main = do   bg <- loadBMP "bg2.bmp"
                rg <- getStdGen
                playIO (InWindow "Circle of Life" (screenx, screeny) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialState     -- Initial state
                        (view bg)        -- View function
                        input            -- Event function
                        (step rg)        -- Step function
