-- | This module contains the data types
--   which represent the state of the game
module Model where
         
data Player = Player { pos :: Position}

data Position = Position { xpos :: Int, ypos :: Int }

data Move = Move { xmov :: Int, ymov :: Int }

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

updatePos :: Player -> Move -> Player
updatePos (Player (Position xpos ypos)) (Move xmov ymov) = Player (Position (xpos+xmov) (ypos+ymov))

data GameState = GameState {
                   elapsedTime :: Float
                 , playerPos :: Position
                 }

initialState :: GameState
initialState = GameState 0 (Position 0 0)