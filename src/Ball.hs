module Ball(
    moveBall,
    ballBounce,
    ballHitEdge) where

import PongGameState
import GameConstants
import Paddle

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
    -> PongGame -- ^ The initial game state
    -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
    where
  
    (x, y)   = ballLoc game    -- The ball location
    (dx, dy) = ballDir game    -- The ball direction vector
    vel      = ballVel game    -- The ball velocity

    -- New locations.
    x' = x + vel * dx * seconds * 10
    y' = y + vel * dy * seconds * 10

ballHitEdge :: PongGame -> PongGame
ballHitEdge game = game {ballLoc = ballLoc', ballVel = bv, ballDir = ballDir', paddleVel = pv}
      where
        (x, y) = ballLoc game
        ballDir'
          | (x + fromIntegral 10) < (fromIntegral (-width)/2) = (1, 0)
          | (x - fromIntegral 10) > (fromIntegral width / 2)  = (-1, 0)
          | otherwise = ballDir game

        ballLoc'
          | (x + fromIntegral 10) < (fromIntegral (-width)/2) = (0, 0)
          | (x - fromIntegral 10) > (fromIntegral width / 2)  = (0, 0)
          | otherwise = ballLoc game

        (pv, bv)
          | (x + fromIntegral 10) < (fromIntegral (-width)/2) ||
           (x - fromIntegral 10) > (fromIntegral width / 2) =
              (minPaddleVel, minBallVel)
          | otherwise = (paddleVel game, ballVel game)


-- | Detect a collision with one of the side walls. Upon collisions,
-- | update the velocity of the ball to bounce it off the wall.
ballBounce :: PongGame -> PongGame
ballBounce game = game { ballDir = ballDir' }
  where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    
    (dx, dy) = ballDir game                                          -- The old direction.
    (bx, by) = ballLoc game                                          -- The Ball Location
    p1 = ( fromIntegral (-width)/2 + paddleOffset, player1 game)     -- player 1 location
    p2 = (fromIntegral width/2 - paddleOffset, player2 game)         -- player 2 location
 
    ballDir' 
        | wallCollision (ballLoc game) ballRadius =  (dx, (-dy)) 
        | paddleCollision game = newDir
        | otherwise = ballDir game
        where
            -- compute the new ball direction
            newDir
                | bx < 0 = calcDirection p1
                |otherwise = calcDirection p2
            -- compute a  velocity vector for the ball 
            calcDirection (x, y) = normalize (nx, ny)

            -- the 30 in the expressions bellow make the vector
            -- to have a 75 degrees when the ball hits a corner
            nx
              | bx < 0 = ((bx - radius) - ((fst p1) - 30))                       -- computes the X-direction based on player 1
              | otherwise = ((bx + radius) - ((fst p2) + 30))                     -- otherwise computes for player 2
            ny
              | bx < 0 = (by - (snd p1))                       -- computes the Y-direction based on player 1
              |otherwise = (by - (snd p2))                     -- otherwise computes for player 2

type Radius = Float 
type Position = (Float, Float)
-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool 
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2 
    bottomCollision = y + radius >=  fromIntegral height / 2

    
-- | normalize a vector
normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = (x / n, y / n) where n = sqrt (x^2 + y^2)