module Ai where

import System.Random
import Moves

type Turn = (Move, Move)

newtype AI = AI {getHistory :: [Turn]}

firstMove :: StdGen -> Move
firstMove = toEnum . fst . randomR (0,2)

lastTurn :: AI -> Turn
lastTurn = head . getHistory

reviewTurn :: Turn -> AI -> AI
reviewTurn turn npc = AI $ turn : getHistory npc

planMove :: AI -> Move
planMove npc = case computerMove `compare` playerMove of
  LT -> computeBeatingMove playerMove
  EQ -> computerMove
  GT -> computeBeatingMove computerMove
  where
    (playerMove, computerMove) = lastTurn npc
    computeBeatingMove move = case move of
      Rock -> Paper
      Paper -> Scissor
      Scissor -> Rock