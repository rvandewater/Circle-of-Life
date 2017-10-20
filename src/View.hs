-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model

--Generate display
view :: GameState -> IO Picture
view gs = {-mappend  (viewIO gs)-} (return (viewPure gs))

--Load background
viewIO :: GameState -> IO Picture
viewIO gs = image

image :: IO Picture
image = loadBMP "bg.bmp"

viewPure :: GameState -> Picture
--Pause scren
viewPure (GameState _ _ _ True _ _ _ _) = translate (-200) 0 (color green (Text "Paused"))

--Default game screen
viewPure gstate = pictures [playerVisual gstate, bulletVisual gstate, enemyVisual gstate]

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual gstate@(GameState s (Player (Position xpos ypos) (HitBox x y) ) _ _ bul _ _ _) = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral x)))

--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual gstate@(GameState _ _ _ _ _ _ enemies _) = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) ) = translate (fromIntegral xpos) (fromIntegral ypos) (color red (rectangleSolid 50 50))

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual gstate@(GameState {bullets}) = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ _ _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)


