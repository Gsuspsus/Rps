module Player 
(
    Player (Player),
    name, 
    score,
    makeMove,
    makeMoves,
    makePlayers,
    Name,
    Score
) where 
import Moves 
import System.Random 

type Name = String
type Score = Int

data Player = Player {name :: Name, score :: Score}

instance Show Player where
    show (Player name score) = name ++ ", Score: " ++ show score

makeMove :: (RandomGen g) => g -> Move
makeMove gen = toEnum move
    where move = fst $ randomR (0,2) gen

makeMoves :: (RandomGen g) => Int -> g -> [Move]
makeMoves n gen = take n moves
    where moves = map toEnum $ randomRs (0,2) gen

makePlayers names = map (\name -> Player name 0) names 