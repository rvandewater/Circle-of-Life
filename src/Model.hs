-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss


data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }


screenx :: Int 
screenx = 720
screeny :: Int
screeny = 1280
circleSize :: Float
circleSize = 20

-- | update position
updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = Position (xpos + xmov) (ypos + ymov)

-- | update position, if it is inside the bounds
updatePosP ::  Move -> Position -> Position
updatePosP  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition = not (outOfBounds (Position newX newY) (HitBox (round circleSize) (round circleSize)))
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

outOfBounds :: Position -> HitBox -> Bool
outOfBounds (Position posx posy) (HitBox width height) =  not validPosition 
  where validPosition :: Bool
        validPosition =    posx <  screenx  - width
                        && posx > -screenx  + width
                        && posy <  screeny  - height
                        && posy > -screeny  + height

data Bullet = Bullet { position:: Position, speed:: Move, size :: HitBox, damage :: Int}

data HitBox = HitBox { width :: Int, height :: Int}
