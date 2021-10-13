module PlayAsWhite where

import State
import Player
import Move

-- Heuristic: prioritises barring the opponent's peices
-- and then scoring points before legal moves.
makeMove :: State -> Lookahead -> Moves
makeMove s l
    -- Checks if there is an available move to make.
    | length (legalMoves s) > 0 = [greedyMove] ++ makeMove (performSingleMove s greedyMove) l
    | otherwise = []
    where
        -- Performs greedy move to obtain the next state.
        s' :: Move -> State
        s' m = performSingleMove s m

        -- Takes the first move from list of barring,
        -- then scoring, or legal moves.
        greedyMove :: Move
        greedyMove = head $ filter isBarring (legalMoves s) ++ filter isScoring (legalMoves s) ++ (legalMoves s)

        -- Checks if the opponent's piece is barred
        -- which would increase their pip value.
        isBarring :: Move -> Bool
        isBarring m
            | opPips (s' m) > opPips s = True
            | otherwise = False

        -- Checks if the score for this player is increased.
        isScoring :: Move -> Bool
        isScoring m
            | myScore (s' m) > myScore s = True
            | otherwise = False

-- Returns the pip value for the opponent.
opPips :: State -> Int
opPips s = case turn s of
    White -> bPips s
    Black -> wPips s

-- Returns the score for this player.
myScore :: State -> Int
myScore s = case turn s of
    White -> wScore s
    Black -> bScore s
