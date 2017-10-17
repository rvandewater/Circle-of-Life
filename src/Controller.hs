-- | This module defines how the state changes
--   in response to time and user input
{-# language NamedFieldPuns #-}
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate@(GameState {playerPos})
   | c == 'w' = gstate { playerPos = Position curposx (curposy+10)  }
   | c == 'a' = gstate { playerPos = Position (curposx-10) curposy  }
   | c == 's' = gstate { playerPos = Position curposx (curposy-10)  }
   | c == 'd' = gstate { playerPos = Position (curposx + 10) curposy  }
     where xpos (Position x y) = x
           ypos (Position x y) = y
           curposx = xpos playerPos
           curposy = ypos playerPos
    -- If the user presses a specific character key, move the player
inputKey _ gstate = gstate -- Otherwise keep the same