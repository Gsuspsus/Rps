import System.Random 
import Moves
import Player
import GameState

main :: IO ()
main = do
    gen <- getStdGen
    let initialState = initialGameState [Player "John" 0, Player "Jane" 6] 5 gen
    gameLoop initialState

gameLoop :: GameState -> IO ()
gameLoop state
    | isFinalState state = putStrLn $ show state
    | otherwise = do 
            putStrLn $ show state 
            gameLoop state