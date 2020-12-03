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
  clearScreen
  setCursorPosition 0 0
  g <- getStdGen
  move <- promptMove
  let aiMove = firstMove g

  gameLoop (GameState move $ AI [(move, aiMove)])

gameLoop :: GameState -> IO ()
gameLoop (GameState playerMove ai) = do
  clearScreen
  setCursorPosition 0 0
  let computerMove = planMove ai
  putStrLn ("You chose " ++ show playerMove)
  putStrLn ("Computer chose " ++ show computerMove)

  announceWinner playerMove computerMove
  setSGR [Reset]

  move <- promptMove
  let ai' = reviewTurn (playerMove, computerMove) ai
  gameLoop (GameState move ai')

announceWinner :: Move -> Move -> IO ()
announceWinner playerMove computerMove
  | playerMove == computerMove = setSGR [SetColor Foreground Vivid Yellow] >> putStrLn "Tie"
  | playerMove > computerMove = setSGR [SetColor Foreground Vivid Green] >> putStrLn "You Won"
  | otherwise = setSGR [SetColor Foreground Vivid Red] >> putStrLn "Computer Won"