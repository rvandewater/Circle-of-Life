-- | This module defines how to turn
--   the game state into a picture
{-# language NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Fixed

--Generate display
view :: Picture -> GameState -> IO Picture
view bg gs@GameState{screen,score} | screen == MainMenu = return (pictures[(scale 0.5 0.5(translate (-fromIntegral screenx) 0 (color green (Text "Circle of life")))), (translate 0 (-200) (color green (thickCircle 10 20)))])
                                   | screen == GameOver      = return (pictures [translate (-200) 0 (color green (Text "Game")), translate (-200) (-200) (color green (Text "Over")),  translate (-200) (-400) (color green (Text (show score)))])
                                   | screen == PausedGame  = return (translate (-200) 0 (color green (Text "Paused")))
                                   | screen == DifficultySelect  = return (scale 0.5 0.5 (pictures [ (translate (- ( fromIntegral screenx)) 0 (color green (Text "Select your difficulty"))), translate 0 (-200) (color green (Text "1")),
                                                        translate 0 (-310) (color green (Text "2")), translate 0 (-420) (color green (Text "3")) ]))
                                   | screen == LevelSelect  = return (scale 0.5 0.5 (pictures [ (translate (- ( fromIntegral screenx)) 0 (color green (Text "Select your level"))), translate 0 (-200) (color green (Text "1")),
                                   translate 0 (-310) (color green (Text "2")), translate 0 (-420) (color green (Text "3")) ]))
                                   | otherwise = return (viewPure bg gs)

viewPure :: Picture -> GameState -> Picture
viewPure bg gstate@GameState{score, screen}             = pictures [updateBg bg gstate, playerVisual gstate, bulletVisual gstate, enemyVisual gstate, informationVisual gstate    ]
--Updating background
updateBg :: Picture -> GameState -> Picture
updateBg bg gs = scale informationScaler 1 (translate 0 (-(mod' (100 * (elapsedTime gs)) 1440) + 720) bg)

--Visualizing player
playerVisual :: GameState -> Picture 
playerVisual GameState{player = Player {pos = Position{xpos, ypos}, hitbox = HitBox{width},hitAnim}} | (hitAnim>0 ) = translate (fromIntegral xpos) (fromIntegral ypos) (color (makeColor hitAnim (1-hitAnim) 0 1) (thickCircle 10 (fromIntegral width)))
                                                                                                     | otherwise = translate (fromIntegral xpos) (fromIntegral ypos) (color green (thickCircle 10 (fromIntegral width)))

--Visualizing each enemy
enemyVisual :: GameState -> Picture
enemyVisual GameState{enemies} = pictures (map enemyPic enemies)

enemyPic :: Enemy -> Picture
enemyPic (Enemy (Position xpos ypos) (HitBox x y) _ _ _ _  epic _ _ _ eanim) = translate (fromIntegral xpos) (fromIntegral ypos) (scale (1-eanim) (1-eanim) epic)

--Visualizing bullets
bulletVisual :: GameState -> Picture 
bulletVisual GameState {bullets} = pictures (map bulletsDraw bullets)

bulletsDraw (Bullet (Position xpos ypos) (BulletType {bulletpic, size = (HitBox x y), speed}) secs) = pictures [ pictures(map (speedLines secs speed) lines), (translate (fromIntegral xpos) (fromIntegral ypos) (bulletpic))]
                where linespaces = map (+ xpos)  (filter even [-(quot x 2).. (quot x 2)])
                      ymap x = (x, ypos) 
                      lines = map ymap linespaces

speedLines :: Float -> Move -> (Int, Int) -> Picture
speedLines secs (Move mx my) (xpos, ypos) = (color white (line [ (fromIntegral xpos, fromIntegral ypos),
        ((fromIntegral xpos), (fromIntegral ypos - secs* (fromIntegral my)))]))
        

informationVisual ::  GameState -> Picture
informationVisual gstate = pictures[ (translate (-  (((fromIntegral informationBar)/2) + fromIntegral screenx/2)) 0 (rotate 90(scale 0.5 0.5(color red (text (show (health(player gstate)))))))), 
     (translate (fromIntegral screenx/2) 0 (rotate 90( scale 0.5 0.5 (color red (text (show (score gstate)))))))]





