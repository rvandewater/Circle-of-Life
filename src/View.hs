-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState s (Position xpos ypos)) = (translate (fromIntegral xpos) (fromIntegral ypos) (color green (circle 80)))