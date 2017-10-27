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
viewPure bg gstate@GameState{lost, paused}  | lost      = pictures [translate (-200) 0 (color green (Text "Game")), translate (-200) (-200) (color green (Text "Over"))]
                                            | paused    = translate (-200) 0 (color green (Text "Paused"))
                                            | otherwise = pictures [updateBg bg gstate, playerVisual gstate, bulletVisual gstate, enemyVisual gstate, informationVisual gstate    ]
--Updating background
updateBg :: Picture -> GameState -> Picture
updateBg bg gs = scale informationScaler 1 (translate 0 (-(mod' (100 * (elapsedTime gs)) 1440) + 720) bg)

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual GameState{player = Player {pos = Position{xpos, ypos}, hitbox = HitBox{width}}} = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral width)))

--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual GameState{enemies} = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _ _  epic _) = translate (fromIntegral xpos) (fromIntegral ypos) (epic)

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual GameState {bullets} = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) _ ) = translate (fromIntegral xpos) (fromIntegral ypos) bulletPic

bulletPic :: Picture
bulletPic = color red (thickCircle 2 5)

informationVisual ::  GameState -> Picture
informationVisual gstate = pictures[ (translate (-  (((fromIntegral informationBar)/2) + fromIntegral screenx/2)) 0 (rotate 90(scale 0.5 0.5(color red (text (show (health(player gstate)))))))), 
     (translate (fromIntegral screenx/2) 0 (rotate 90( scale 0.5 0.5 (color red (text (show (score gstate)))))))]





