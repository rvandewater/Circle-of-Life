module Main where
import Graphics.Gloss
drawing :: Picture
drawing = (pictures [  translate 0 (-300) (Circle 80), translate (-500) 0 (Text "The circle of life")])
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white drawing
    

