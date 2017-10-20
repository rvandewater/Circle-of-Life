-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss


data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

<<<<<<< HEAD
data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }
nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5
=======
data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool }
>>>>>>> origin/master

screenx :: Int 
screenx = 720
screeny :: Int
screeny = 1280
circleSize :: Float
circleSize = 20

-- | update position, if it is inside the bounds
updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition =    newX + newX <  screenx - round circleSize 
                        && newX + newX > -screenx + round circleSize 
                        && newY + newY <  screeny - round circleSize 
                        && newY + newY > -screeny + round circleSize 
        newX          = xpos + xmov
        newY          = ypos + ymov


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
movementSpeed = 10

bulletSpeed :: Move
bulletSpeed = Move 0 5

bulletBox :: HitBox
bulletBox = HitBox 10 10 

bulletDamage :: Int
bulletDamage = 5

data Bullet = Bullet { position:: Position, speed:: Move, size :: HitBox, damage :: Int}

data HitBox = HitBox { width :: Int, height :: Int}
