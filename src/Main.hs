module Main where
    
    import Controller
    import Model
    import View
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    
    main :: IO ()
    main = do   bg <- loadBMP "bg.bmp"
                playIO (InWindow "Circle of Life" (screenx, screeny) (0, 0)) -- Or FullScreen
                        black            -- Background color
                        60               -- Frames per second
                        initialState     -- Initial state
                        (view bg)        -- View function
                        input            -- Event function
                        step             -- Step function
