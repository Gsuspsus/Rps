module Player 
(
    Player (Player),
    name, 
    score,
    makeMove,
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
makeMove gen = (toEnum (fst $ randomR (0,2) gen))