import System.Random 
import Control.Monad
import Moves

humanSelect :: IO Move
humanSelect = fmap read getLine

computerSelect :: IO Move
computerSelect = fmap toEnum (randomRIO (0,2))

main :: IO ()
main = do
    putStrLn "Select Move"
    playerMove <- humanSelect
    computerMove <- computerSelect

    putStrLn ("You chose " ++ show playerMove)
    putStrLn ("Computer chose " ++ show computerMove)

    if playerMove == computerMove then 
        putStrLn "Tie"
    else if playerMove > computerMove then 
        putStrLn "You Won"
    else 
        putStrLn "Computer Won"

    main