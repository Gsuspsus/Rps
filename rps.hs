import Ai
import Control.Monad
import Moves
import System.Console.ANSI
import System.Random

data GameState = GameState Move AI

promptMove :: IO Move
promptMove = do
  putStrLn "Select Move"
  read <$> getLine


main :: IO ()
main = do
  g <- getStdGen
  move <- promptMove
  let aiMove = firstMove g

  gameLoop (GameState move $ AI [(move,aiMove)] )

gameLoop :: GameState -> IO ()
gameLoop (GameState playerMove ai) = do
  let computerMove = planMove ai
  putStrLn ("You chose " ++ show playerMove)
  putStrLn ("Computer chose " ++ show computerMove)

  if playerMove == computerMove
    then putStrLn "Tie"
    else
      if playerMove > computerMove
        then putStrLn "You Won"
        else putStrLn "Computer Won"

  move <- promptMove
  gameLoop (GameState move (reviewTurn (playerMove,computerMove) ai))