-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

-- ********************* DATA TYPES ***************
data GameState = GameState {
                   elapsedTime :: Float
                 , player :: Player
                 , keys :: KeysPressed
                 , paused :: Bool
                 , lost :: Bool
                 , bullets :: [Bullet]
                 , enemies :: [Enemy]
                 , level :: Int
                 }

data Bullet = Bullet { position:: Position, kind :: BulletType}

data BulletType =  BulletType { speed:: Move, size :: HitBox, damage :: Int}

data HitBox = HitBox { width :: Int, height :: Int}   

data Player = Player { pos :: Position, hitbox :: HitBox, fireRate :: Float, bullet :: BulletType, lastFire :: Float }

data Enemy = Enemy { enemypos :: Position, enemyhitbox :: HitBox, enemyfireRate :: Float, enemyBullet :: BulletType, enemyLastFire :: Float }

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

data KeysPressed = KeysPressed { w :: Bool, a :: Bool, s :: Bool, d:: Bool, space :: Bool }


-- ********************* FUNCTIONS ***************

-- | update position
updatePos ::  Move -> Position -> Position
updatePos  (Move xmov ymov) (Position xpos ypos) = Position (xpos + xmov) (ypos + ymov)

-- | update position, if it is inside the bounds
updatePosP ::  Move -> Position -> Position
updatePosP  (Move xmov ymov) (Position xpos ypos) = if validPosition then Position newX newY else Position xpos ypos
  where validPosition :: Bool
        validPosition = not (outOfBounds (Position newX newY) playerHitBox)
        newX          = xpos + xmov
        newY          = ypos + ymov

outOfBounds :: Position -> HitBox -> Bool
outOfBounds (Position posx posy) (HitBox width height) =  not validPosition 
  where validPosition :: Bool
        validPosition =    posx + posx <  screenx - width
                        && posx + posx> -screenx  + width
                        && posy + posy <  screeny - height
                        && posy + posy > -screeny + height

-- ********************* CONSTANTS ***************
initialState :: GameState
--Starting phase
initialState = GameState 0 (Player beginPos playerHitBox 1 standardBullet 0) (KeysPressed False False False False False) False False [] [Enemy beginPos playerHitBox 0 standardBullet 0] 1

standardBullet :: BulletType
standardBullet = (BulletType bulletSpeed bulletHitBox bulletDamage)

--moventSpeed of the player
movementSpeed :: Int
movementSpeed = 10

bulletSpeed :: Move
bulletSpeed = Move 0 5

bulletHitBox :: HitBox
bulletHitBox = HitBox 10 10 

bulletDamage :: Int
bulletDamage = 5

screenx :: Int 
screenx = 720

screeny :: Int
screeny = 1280

playerHitBox :: HitBox
playerHitBox = HitBox 20 20

beginPos :: Position
beginPos = Position 0 0
