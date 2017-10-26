-- | This module contains the data types
--   which represent the state of the game
{-# language NamedFieldPuns #-}
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

data BulletType =  BulletType { speed:: Move, size :: HitBox, damage :: Float}

data HitBox = HitBox { width :: Int, height :: Int}   

data Player = Player { pos :: Position, hitbox :: HitBox, fireRate :: Float, bullet :: BulletType, lastFire :: Float, health :: Float }

data Enemy = Enemy { epos :: Position, ehitbox :: HitBox, efireRate :: Float, eBullet :: BulletType, eLastFire :: Float, ehealth :: Float }

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
                        && posx + posx> - screenx  + width
                        && posy + posy <  screeny - height
                        && posy + posy > -screeny + height
class Ship k where 
  isHit :: k -> Bullet -> Float

instance Ship Player where 
  isHit player@(Player {pos, hitbox, health}) (Bullet bulletpos (BulletType _ size dmg))    | collision (hitbox, pos) (size, bulletpos) = dmg
                                                                                            | otherwise = 0
instance Ship Enemy where
  isHit enemy@(Enemy {epos, ehitbox, ehealth}) (Bullet bulletpos (BulletType _ size dmg))    | collision (ehitbox, epos) (size, bulletpos) = dmg
                                                                                             | otherwise = 0

collision:: (HitBox, Position) -> (HitBox, Position) -> Bool
collision (HitBox w1 h1, Position x1 y1) (HitBox w2 h2, Position x2 y2) = (x1  < x2 + w2) &&
                                                                          (x1 + w1 > x2) &&
                                                                          (y1 < y2 + h2) &&
                                                                          (y1 + h1 > y2)

-- ********************* CONSTANTS ***************
initialState :: GameState
--Starting phase
initialState = GameState 0 (Player beginPos playerHitBox 1 standardBullet 0 100) (KeysPressed False False False False False) False False [] [Enemy beginPos playerHitBox 0 standardBullet 0 100] 1

standardBullet :: BulletType
standardBullet = (BulletType bulletSpeed bulletHitBox bulletDamage)

--moventSpeed of the player
movementSpeed :: Int
movementSpeed = 10

bulletSpeed :: Move
bulletSpeed = Move 0 5

bulletHitBox :: HitBox
bulletHitBox = HitBox 10 10 

bulletDamage :: Float
bulletDamage = 10

screenx :: Int 
screenx = 608

screeny :: Int
screeny = 1080

playerHitBox :: HitBox
playerHitBox = HitBox 20 20

beginPos :: Position
beginPos = Position 0 0
