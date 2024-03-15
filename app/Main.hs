import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Time.Clock.POSIX

data GameState = GameState
  { playerX          :: Float
    , playerY        :: Float
    , playerVelocity :: Float
    , obstacles      :: [Float]
    , score          :: Int
    , startTime      :: Int
    , obstacleSpeed  :: Float
    , gameOver       :: Bool
   }

initialGameState :: IO GameState
initialGameState = do
    currentTime <- round <$> getPOSIXTime
    let startTime = currentTime
    return GameState
      {  playerX         = 0
        , playerY        = 0
        , playerVelocity = 0
        , obstacles      = [300]
        , score          = 0
        , startTime      = startTime
        , obstacleSpeed  = 100
        , gameOver       = False
      }

window :: Display
window = InWindow "Dodge the Obstacles" (800, 400) (10, 10)

gravity :: Float
gravity = -500

floorY :: Float
floorY = -160


update :: Float -> GameState -> IO GameState
update dt gs@(GameState px py pv obs sc st obsSpeed gameOver)
    | gameOver = return gs
    | otherwise = do
        -- Apply gravity to the player's vertical velocity.
        let newPV = pv + gravity * dt
        -- Calculate the new player position.
        let newPY = py + newPV * dt
        -- Keep the player on the floor.
        let clampedPY = max newPY floorY
        -- Move the obstacles.
        let newObs = map (\x -> x - obsSpeed * dt) obs
        -- Generate new obstacles.
        newObs' <- generateObstacles newObs
        -- Check for collisions while on the ground.
        let onGroundCollision = any (\obstacleX -> abs (px - obstacleX) < 30 && clampedPY == floorY) newObs'
        -- Calculate the score based on how many obstacles the player dodged.
        let newScore = length $ filter (\x -> x < px) newObs'
        return $ if onGroundCollision
            then gs { gameOver = True, score = newScore }
            else gs { playerY = clampedPY, playerVelocity = newPV, obstacles = newObs', score = sc, startTime = st, obstacleSpeed = obsSpeed }



generateObstacles :: [Float] -> IO [Float]
generateObstacles obs = do
    rand <- randomRIO (0, 1) :: IO Float
    let newObs = if rand < 0.02 then 800 : obs else obs -- Randomly generate a new obstacle at the right edge of the screen with a small probability.
    return $ filter (\x -> x > -400) newObs -- Remove any obstacle that is too far to the left.

handleInput :: Event -> GameState -> IO GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _ ) gs@(GameState px py pv _ sc st obsSpeed gameOver) =
    return $ if py <= floorY + 1 && not gameOver
        then gs { playerVelocity = 300, score = sc, startTime = st, gameOver = False }
        else gs
handleInput _ gs = return gs -- Catch-all pattern to handle other events


render :: GameState -> IO Picture
render (GameState playerX playerY playerVelocity obstacles score startTime obstacleSpeed gameOver) =       
        return $ Pictures
        [ Translate playerX playerY $ Color blue $ rectangleSolid 40 40 -- Draw the player as a blue square.
         , Pictures [Translate x (-160) $ Color red $ rectangleSolid 20 20 | x <- obstacles] -- Draw the obstacles as red squares.
         ,   Translate (-380) 180 $ Scale 0.1 0.1 $ Color green $ Text $ "Score: " ++ show score -- Show the current score at the top left corner.
         , if gameOver  -- If the game is over, show a message in the center of the screen.
                then Translate (-100) 0 $ Scale 0.2 0.2 $ Color red $ Text "Game Over!"
         else Blank -- Otherwise, show nothing.
        ]

main :: IO ()
main = do
    initialGame <- initialGameState -- Initialize the game state.
    playIO window white 100 initialGame render handleInput update -- Start the game loop with a frame rate of 60 FPS.