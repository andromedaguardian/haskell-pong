module PongGameState(
    PongGame(Game),
    ballLoc,
    ballVel,
    ballDir,
    player1,
    player2,
    dp1,
    dp2,
    paddleHits,
    paddleVel,
    difficult,
    render) where

import Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

import GameConstants
-- import Paddle
-- import Ball
-- import Joystick

-- | A data structure to hold the state of the Pong game.
-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc :: (Float, Float)  -- ^ Pong ball (x, y) location.
  , ballVel :: Float           -- ^ Pong ball (x, y) velocity.
  , ballDir :: (Float, Float)  -- ^ A unit vector describing  the ball direction.
  , player1 :: Float           -- ^ Left player paddle height.
                               -- Zero is the middle of the screen. 
  , player2 :: Float           -- ^ Right player paddle height.
  , dp1   :: Float             -- ^ Player 1 direction
  , dp2   :: Float             -- ^ Player 2 direction
  , paddleHits :: Integer      -- ^ Number of times the ball hits a paddle.
  , paddleVel  :: Float        -- ^ Paddle moviment velocity
  , difficult  :: Int          -- ^ game level
  } deriving Show



-- | Draw a pong game state (convert it to a picture).
-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
-- render game = renderLevel (difficult game) game
render game = 
  pictures [ball,
            mkPaddle paddleColor (fromIntegral (-width)/2 + paddleOffset) $ player1 game,
            mkPaddle paddleColor (fromIntegral width/2 - paddleOffset) $ player2 game,
            levels (difficult game)]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
      [translate x y $ color paddleColor $ rectangleSolid paddleWidth paddleHeight]

    paddleColor = light (light blue)



    -- | this function designs the screen for level 1
levels :: Int -> Picture
levels n
    | n < length allLevels = pictures (Prelude.take n allLevels)
    | otherwise = pictures (Prelude.take (length allLevels) allLevels) 
    where allLevels = Prelude.map (\(p ,d) -> translate (fst p) (snd p) $ d) gameElements
          gameElements = zip blocks gameBlocks
          gameBlocks = replicate (length blocks) (color orange $ rectangleSolid blockWidth blockHeight) 