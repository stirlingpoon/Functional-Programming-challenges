{-|
A simple Negamax with alpha beta pruning that searches until a depth of 8 (<30s)
-}
module Challenge2 where

import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Debug.Trace

import Game
import GameUtils
import System.Random

-- OPTION 1: pure (stateless) implemenetation
example1 :: Player -> Board -> Move
example1 p b = R 1

exampleStrategy1 = simpleStrategy example1

-- OPTION 2: stateful implementation
type ExampleState = Int
exampleComputeMove :: RandomGen g => g -> ExampleState -> Player -> Board -> (Move, ExampleState)
exampleComputeMove gen s pl board = (L s, (s+1) `mod` boardHeight + 1)

exampleInitialState :: RandomGen g => g -> Player -> ExampleState
exampleInitialState gen pl = fst $ randomR (1, boardHeight) gen

exampleStrategy2 :: Strategy ExampleState
exampleStrategy2 = Strategy exampleInitialState exampleComputeMove

-- YOUR AI STRATEGY (see examples below how to implement it)
-- history is a list of board states, with the first one being the current board
type History = [Board]
data State = State History

-- Return all possible move [L1, R1, T1, B1 ..., L4, R4, T4, B4]
moves :: [Move]
moves = concat [[L n, R n, T n, B n] | n <- [1..boardHeight]]

-- makeMove assuming the move is valid
makeMove' :: Player -> Move -> Board -> Board
makeMove' pl mv board = fromJust $ makeMove pl mv board

nextBoards pl board = [(mv, makeMove' pl mv board) | mv <- moves]

nextBoardsUnique' pl board (mv:xs) = if any (==board') $ map snd rest then rest else (mv, board'):rest
	where
		board' = makeMove' pl mv board
		rest = nextBoardsUnique' pl board xs
nextBoardsUnique' pl board [] = []

nextBoardsUnique pl board = nextBoardsUnique' pl board moves

-- Has the board not previously appeared?
previousBoard :: History -> Board -> Bool
previousBoard hs board = if length hs == 0 then False else any (==board) $ tail hs

-- Score the board state
getScore :: Player -> History -> Board -> Int
getScore pl hs board
	-- Previous state loses
	| previousBoard hs board 		= -1
	-- More straights wins
	| hasWon pl board 				= 1
	-- Less straights loses
	| hasWon (otherPlayer pl) board = -1
	-- Neutral board
	| otherwise 					= 0

-- fst/snd only works on (x, y) pairs
fst3 (x, y, z) = x
snd3 (x, y, z) = y
thd3 (x, y, z) = z

inf = 2^30


-- Negamax with alpha beta pruning
-- Modified to retain the move associated with the best score
-- Taken from https://en.wikipedia.org/wiki/Negamax#NegaMax_with_Alpha_Beta_Pruning
{-
negamax :: Player -> History -> Move -> Board -> Int -> Int -> Int -> (Int, Char, Move)
negamax pl hs mv board depth a b
	-- Depth exceeded (terminal node), stop searching
	| depth > 2		= (0, 'd', mv)
	-- Win/lose condition (terminal node), stop searching
	| score /= 0	= debug (score, 't', mv)
	-- Do a alpha-beta pruning of the child negamax searches
	| otherwise		= debug $ fst3 $ foldl abPrune ((-inf, 'b', (L 0)), a, b) $ nextBoards pl board
	where
		-- Raw score of the board (continue searching if = 0)
		score = getScore pl hs board
		debug r = trace ("negamax: " ++ show pl ++ " depth " ++ show depth ++ ": " ++ show r) r
		-- Alpha beta pruning of child negamax values
		abPrune :: ((Int, Char, Move), Int, Int) -> (Move, Board) -> ((Int, Char, Move), Int, Int)
		abPrune ((best, r, bestMv), a, b) (mv, board)
			| a >= b 	= ((best, r, bestMv), a, b)
		  	| otherwise = trace ("val: " ++ show pl ++ " depth " ++ show depth ++ ": " ++ show val) (max (best, r, bestMv) val, max a $ fst3 val, b)
		  	where
		  		val = negate' $ negamax (otherPlayer pl) (board:hs) mv board (depth+1) (negate b) (negate a)
		  		negate' (n, r, x) = (-n, r, x)
-}

bestMove :: Player -> History -> Board -> Move
bestMove pl hs board = snd $ negamax pl hs (L 0) board 2 (-inf) inf
    where
    	negamax pl hs mv board depth a b
              | depth == 0 = trace (show depth ++ " " ++ show pl ++ " " ++ show (score, mv)) (score, mv)
              | otherwise = trace (show depth ++ " " ++ show pl ++ " " ++ show childScore) childScore
              where
              	score = getScore pl hs board
              	childScore = fst3 $ foldl abPrune ((-inf, L 0), a, b) $ nextBoards pl board
                abPrune (best, a, b) (mv, board)
                	| a >= b = (best, a, b)
                	| otherwise = (best', a', b)
                	where
                    	val = negate' $ negamax (otherPlayer pl) (board:hs) mv board (depth-1) (negate b) (negate a)
                        best' = max best val
                        a' = max a $ fst val
                        negate' (n, x) = (-n, x)

{-
randomElement :: RandomGen g => g -> [a] -> a
randomElement gen xs = xs !! (fst $ randomR (0, length xs - 1) gen)
-}

testBoard = makeMove' Cross (B 4) $ makeMove' Cross (B 4) $ makeMove' Nought (T 3) $ makeMove' Nought (T 2) $ makeMove' Nought (T 1) emptyBoard

customComputeMove :: RandomGen g => g -> State -> Player -> Board -> (Move, State)
customComputeMove gen (State hs) pl board = (mv, (State (newBoard:board:hs)))
	where
		mv = bestMove pl hs board
		newBoard = makeMove' pl mv board



customInitialState :: RandomGen g => g -> Player -> State
customInitialState gen pl = State []

strategy = Strategy customInitialState customComputeMove