module Paddle(movePaddle, paddleConstraints, paddleHitBall, paddleCollision)where

import PongGameState
import GameConstants

-- | Update all players position using its current Y-direction.
movePaddle :: PongGame -> PongGame
movePaddle game = game { player1 = p1, player2 = p2}
    where
        p1 = (player1 game) + (paddleVel game) * (dp1 game)
        p2 = (player2 game) + (paddleVel game) * (dp2 game)

-- | Detect if a paddle hit the top/bottom edge. If hit
-- | it stops to move at that direction
paddleConstraints :: PongGame -> PongGame
paddleConstraints game = game {player1 = p1, player2 = p2}
    where
        p1
            | paddleHitTop (player1 game) =
                fromIntegral height / 2 - paddleHeight / 2
            | paddleHitBottom (player1 game) =
                fromIntegral (-height) / 2 + paddleHeight / 2
            | otherwise = (player1 game)
        p2
            | paddleHitTop (player2 game) =
                fromIntegral height / 2 - paddleHeight / 2
            | paddleHitBottom (player2 game) =
                fromIntegral (-height) / 2 + paddleHeight / 2
            | otherwise = (player2 game)


paddleHitTop :: Float -> Bool
paddleHitTop y
    | y > ((fromIntegral height / 2) - paddleHeight / 2) = True
    | otherwise = False

paddleHitBottom :: Float -> Bool
paddleHitBottom y
    | y < (fromIntegral (-(height)) / 2 + paddleHeight / 2) = True
    | otherwise = False


-- | update the number of times the ball hits any paddle
paddleHitBall :: PongGame -> PongGame
paddleHitBall game = game { paddleHits = pHits}
    where 
        pHits = if paddleCollision game 
                  then paddleHits game + 1
                  else paddleHits game


-- | Check if the ball hits a paddle(left or right). this function implements
-- | a simple AABB collision
paddleCollision :: PongGame -> Bool
paddleCollision game
    -- checking if the ball overlap the left paddle
    | (x1 + w) > (bx - ballRadius) && (x1 - w) < (bx + ballRadius) &&
      (y1 + h) > (by - ballRadius) && (y1 - h) < (by + ballRadius) =
        True
    -- checking if the ball overlap the rigth paddle    
    | (x2 - w) < (bx + ballRadius) && (x2 + w) > (bx - ballRadius) &&
      (y2 + h) > (by - ballRadius) && (y2 - h) < (by + ballRadius) =
          True
    -- if the ball isn't overlapping a paddle 
    | otherwise = False
    where
        x1 = (fromIntegral (-width)/2 + paddleOffset)  -- Player 1 X-coordinate
        x2 = (fromIntegral width/2 - paddleOffset)     -- Player 2 X-coordinate
        y1 = (player1 game)                            -- Player 1 Y-coordinate
        y2 = (player2 game)                            -- Player 2 Y-coordinate
        (bx , by) = (ballLoc game)                     -- Ball (x, y) Location
        w = paddleWidth / 2                            -- a half of the paddle total width
        h = paddleHeight / 2                           -- a half of the paddle total height