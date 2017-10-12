module Main where
import Graphics.Gloss
drawing :: Picture
drawing = (pictures [  translate 0 (-300) (Circle 80), translate (-500) 0 (Text "The circle of life")])
main = animate (InWindow "Nice Window" (200, 200) (10, 10)) white (\t ->  
    (pictures [ (translate 0 (-300) (Circle 80)),
    (rotate (60*t) (translate (-500) 0 (Text "The circle of life")))]))
    

