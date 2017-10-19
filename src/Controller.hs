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
step secs gstate@GameState { paused }
  = if (not paused) then return $ gstate { elapsedTime = elapsedTime gstate + secs                          --update time
                                           ,playerPos = (movementUpdate (keys gstate) (playerPos gstate)) } --update player position
                    else return   gstate

-- | Updating Position depending on keys pressed
movementUpdate :: KeysPressed -> Position -> Position
movementUpdate (KeysPressed w a s d) =  boolupdate w (Move 0 movementSpeed). 
                                        boolupdate a (Move (- movementSpeed) 0).
                                        boolupdate s (Move 0 (- movementSpeed)).
                                        boolupdate d (Move movementSpeed 0)
                                where     boolupdate True mov = updatePos mov
                                          boolupdate False _ = id
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- | Handle input of a key
inputKey :: Event -> GameState -> GameState
{-inputKey (EventKey (Char c) _ _ _) gstate@(GameState {playerPos})
   | c == 'w' = gstate { playerPos = (updatePos playerPos (Move 0 movementSpeed))  }
   | c == 'a' = gstate { playerPos = Position (curposx-10) curposy  }
   | c == 's' = gstate { playerPos = Position curposx (curposy-10)  }
   | c == 'd' = gstate { playerPos = Position (curposx + 10) curposy  }
     where xpos (Position x y) = x
           ypos (Position x y) = y
           curposx = xpos playerPos
           curposy = ypos playerPos -}
inputKey (EventKey (Char 'p') Down _ _) gstate@GameState { keys, paused }= gstate {paused = not paused}
inputKey (EventKey (Char c) _ _ _) gstate@GameState { keys, paused }= gstate {keys = (updatePress c keys )}
{-
inputKey (EventKey  _ _ _ _) gstate@(GameState {playerPos}) = 
  gstate { playerPos = (updatePos playerPos (Move 0 movementSpeed))  -}
    -- If the user presses a specific character key, move the player
inputKey _ gstate = gstate -- Otherwise keep the same 

-- | Updating keyspressed depending on which key is pressed
updatePress :: Char -> KeysPressed -> KeysPressed
updatePress 'w' keys@KeysPressed {w} = keys { w = not w }
updatePress 'a' keys@KeysPressed {a} = keys { a = not a }
updatePress 's' keys@KeysPressed {s} = keys { s = not s }
updatePress 'd' keys@KeysPressed {d} = keys { d = not d }
updatePress  _  x                    = x

