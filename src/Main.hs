module Main(main) where

import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import GameConstants

import PongGameState
import Ball
import Paddle
import Joystick


-- | Initialize the game with this game state.
-- | The starting state for the game of Pong.
initialAngle :: Float
initialAngle = degreeToRadius 0              -- an angle in degree

initialBallDir :: (Float, Float)
initialBallDir =  (cos (initialAngle), sin (initialAngle))

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = minBallVel
  , ballDir = initialBallDir
  , player1 = fromIntegral height / 2 - paddleHeight
  , player2 = fromIntegral (-height) / 2 + paddleHeight
  , dp1 = 0
  , dp2 = 0
  , paddleHits = 0
  , paddleVel = minPaddleVel
  , difficult = 0
  }

window :: Display
window = InWindow "Pong" (width, height) (offset, offset)

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update  seconds = 
     paddleConstraints . paddleHitBall . movePaddle .  pongLevelUp . ballHitEdge . ballBounce . moveBall seconds

-- | convert degrees to radius
degreeToRadius :: Float -> Float
degreeToRadius x = x * pi / 180
    