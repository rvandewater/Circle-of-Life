-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss


data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = Position (xpos+xmov) (ypos+ymov)
--GameState variable in which 
data GameState = GameState {
                   elapsedTime :: Float
                 , playerPos :: Position
                 , keys :: KeysPressed
                 , paused :: Bool
                 , bullets :: [Bullet]
                 }

initialState :: GameState
--Starting phase
initialState = GameState 0 (Position 0 0) (KeysPressed False False False False False) False []

--moventSpeed of the player
movementSpeed :: Int
movementSpeed = 5

bulletSpeed :: Move
bulletSpeed = Move 0 5

bulletBox :: HitBox
bulletBox = HitBox 10 10 

bulletDamage :: Int
bulletDamage = 5

data Bullet = Bullet { position:: Position, speed:: Move, size :: HitBox, damage :: Int}

data HitBox = HitBox { width :: Int, height :: Int}