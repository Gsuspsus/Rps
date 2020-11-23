import System.Random 
import Text.Read
import Control.Monad
import Data.Maybe
import Moves


humanSelect :: IO (Maybe Move)
humanSelect = readMaybe <$> getLine

computerSelect :: IO Move
computerSelect = toEnum <$> randomRIO (0,2)

main :: IO ()
main = do
    putStrLn "Select Move"
    maybeMove <- humanSelect
    computerMove <- computerSelect

    when (isNothing maybeMove) $ do
        putStrLn "Invalid move, try again"
        main

    let playerMove = fromJust maybeMove

    putStrLn ("You chose " ++ show playerMove)
    putStrLn ("Computer chose " ++ show computerMove)

    if playerMove == computerMove then 
        putStrLn "Tie"
    else if playerMove > computerMove then 
        putStrLn "You Won"
    else 
        putStrLn "Computer Won"

    main