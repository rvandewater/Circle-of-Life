-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState _ _                     _ True) =  translate (-200) 0 (color green (Text "Paused"))
viewPure (GameState s (Position xpos ypos)  _ _)    = (translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 20 40)))


