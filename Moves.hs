module Moves 
(
    Move (Rock,Paper,Scissor)
)where

data Move = Rock | Paper | Scissor deriving (Show, Eq, Read,Enum)

instance Ord Move where 
    compare Rock Paper = LT
    compare Rock Scissor = GT

    compare Paper Scissor = LT
    compare Paper Rock = GT

    compare Scissor Rock = LT
    compare Scissor Paper = GT 

    compare x y 
        | x == y = EQ
