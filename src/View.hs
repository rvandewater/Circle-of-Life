-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Fixed

--Generate display
view :: Picture -> GameState -> IO Picture
view bg gs = return (viewPure bg gs)

viewPure :: Picture -> GameState -> Picture

--Game over screen
viewPure _ (GameState _ _ _ _ True _ _ _) = pictures [translate (-200) 0 (color green (Text "Game")), translate (-200) (-200) (color green (Text "Over"))]

--Pause screen
viewPure _ (GameState _ _ _ True _ _ _ _) = translate (-200) 0 (color green (Text "Paused"))

--Default game screen
viewPure bg gstate = pictures [updateBg bg gstate, playerVisual gstate, bulletVisual gstate, enemyVisual gstate,   (color red (text (show (health(player gstate)))))]

--Updating background
updateBg :: Picture -> GameState -> Picture
updateBg bg gs = translate 0 (-(mod' (100 * (elapsedTime gs)) 1440) + 720) bg

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual gstate@(GameState s (Player (Position xpos ypos) (HitBox x y) _ _ _ _) _ _ bul _ _ _) = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral x)))

--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual gstate@(GameState _ _ _ _ _ _ enemies _) = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _ _) = translate (fromIntegral xpos) (fromIntegral ypos) (color red (rectangleSolid 50 50))

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual gstate@(GameState {bullets}) = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)


