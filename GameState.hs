module GameState
(
    GameState,
    initialGameState, 
    isFinalState 
)
where

import Player
import Data.List
import Data.Maybe
import System.Random

data GameState = GameState {players :: [Player], goal :: Score}

instance Show GameState where
    show state 
        |isFinalState state = "Winner is " ++ show (fromJust $ getWinner state)
        |otherwise = "Players:\n" ++ (intercalate " ; " $ map show $ players state)

initialGameState :: [Player] -> Score -> StdGen -> GameState
initialGameState  players goal gen = GameState players goal --gen

isFinalState :: GameState -> Bool
isFinalState = not . isNothing . getWinner

getWinner :: GameState -> Maybe Player
getWinner state = find (\p -> score p >= goal state) $ players state

--nextState :: (StdGen g) => GameState -> g -> GameState
--nextState state gen = if makeMove
    