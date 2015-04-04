{-|
Module      : Game
Description : Provides some data types and auxiliary functions for the "connect two sides game".
Maintainer  : Tomas Tauber

Thanks to Jinxu Zhao for a patch.

This module provides some data types and auxiliary functions for the "connect two sides game".
A game board is represented by the data type @Board@. A @Board@ has positions from @(1,1)@ to
@(boardWidth, boardHeight)@, i.e. from @(1,1)@ to @(4,4)@. Every position can either contain a Nought,
a Cross, or nothing at all. This is represented by
@Just Nought@ ("◯"), @Just Cross@ ("╳"), and @Nothing@ ("⬚") respectively.

Initially the board is empty, and the players take turns inserting
their markers on the board.
The player with Cross always goes first.
The columns and rows are numbered from 1 to N, starting from the top left, as in:

@
   1 2 3 4
  +-+-+-+-+
1 | | | | |
  +-+-+-+-+
2 | | | | |
  +-+-+-+-+
3 | | | | |
  +-+-+-+-+
4 | | | | |
  +-+-+-+-+
@

A marker can only be inserted on the board by sliding it onto a particular
row from the left or from the right, or onto a particular column from the top
or from the bottom. So, there are 4*N possible "moves" (ways to insert a marker).
They are named L i, R i, T i, and B i respectively,
where i is the number of the row or column where the insertion takes place.

When a marker is inserted, there may be a marker on the square where
the insertion takes place. In this case, all markers on the insertion
row or column from the insertion square upto the first empty square are moved
one square further to make room for the inserted marker.
Note that the last marker of the row or column will be pushed off the board
(and must be removed from play) if there are no empty squares on the insertion row or column.

A row or a column is a straight of a given symbol if it contains N markers of the given symbol.

The game ends either when an insertion:

* repeats a previous configuration of the board;
in this case the player who inserted the marker LOSES.

* creates a configuration with more straights of one marker than straights of the other marker;
the player whose marker is dominant (in number of straights) WINS.

A game always leads to a win by one of the two players. Draws are impossible.

(Text adapted from PLT Games documentation
code adapted from original code for Othello by Manuel Eberl.)
-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module Game(
-- * Game representation
-- ** Types
Position, Player (Nought, Cross), Board (Board), Move, Direction(L,R,T,B),
-- ** Inspecting and modifying boards
otherPlayer, boardWidth, boardHeight, boardBounds, emptyBoard, isValidPosition, playerAt,
isValidMove, makeMove, showBoard, readBoard, hasWon, getRow, getColumn,
-- * Strategies
Strategy (Strategy, initialState, computeMove), simpleStrategy
)  where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Ord
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Array (Array, (!), (//))
import Data.Ix
import qualified Data.Array as A
import System.Random

{- Representation of players and the board -}

type Position   = (Int, Int)

-- | A player in the game.
data Player     = Nought | Cross deriving (Eq, Ord, Show, Read)

-- | Moves in the game.
data Direction a = L a | R a | T a | B a deriving (Eq, Ord, Show, Read)
type Move      = Direction Int

-- | A board of stones, each position containing either @Just@ a @Player@ or @Nothing@.
newtype Board   = Board (Array Position (Maybe Player)) deriving (Eq)

-- | Returns the opposite player.
otherPlayer :: Player -> Player
otherPlayer Nought  = Cross
otherPlayer Cross   = Nought

-- | The width and height of the board.
boardDimensions :: (Int, Int)
boardDimensions = (4, 4)

-- | The width of the board.
boardWidth = fst boardDimensions

-- | The height of the board.
boardHeight = snd boardDimensions

-- | Lower/upper bound of the game.
boardBounds :: ((Int,Int), (Int,Int))
boardBounds = ((1, 1), boardDimensions)

-- | The empty board.
emptyBoard :: Board
emptyBoard = Board (A.listArray boardBounds (replicate (rangeSize boardBounds) Nothing))

-- | Returns whether a position is within the bounds of the board.
isValidPosition :: Position -> Bool
isValidPosition = inRange boardBounds

-- | Returns the player who owns the stone at the given position (or Nothing if there is no stone).
playerAt :: Position -> Board -> Maybe Player
playerAt p (Board a) = if isValidPosition p then a ! p else Nothing

-- | Displays a board as a String. Empty rectangles denote an empty position,
--   circles denote a Nought and crosses denote a Cross.
showBoard board = intercalate "\n" [showLine y | y <- [1..boardHeight]]
    where showLine y = intercalate " " [showPos (x,y) | x <- [1..boardWidth]]
          showPos pos = case playerAt pos board of
                          Nothing   -> "⬚"
                          Just Nought  -> "◯"
                          Just Cross -> "╳"

readBoard s =
  Board . A.listArray boardBounds . concat . transpose . reverse . map (map f . filter (not . isSpace)) $ lines s
  where f '⬚' = Nothing
        f '◯' = Just Nought
        f '╳' = Just Cross
        f _   = error "Game.readBoard: No parse."

instance Show Board where
  show b = "\n" ++ showBoard b ++ "\n"

-- | Returns whether a given push is a valid move.
isValidMove :: Move -> Bool
isValidMove m =
  case m of (T n) -> inRange (1,boardWidth) n
            (B n) -> inRange (1,boardWidth) n
            (L n) -> inRange (1,boardHeight) n
            (R n) -> inRange (1,boardHeight) n

-- | Returns a list of positions and markers at the given row.
getRow :: Int -> Board -> [(Position, Maybe Player)]
getRow n (Board b) = filter ((n==) . snd . fst) $ A.assocs b

-- | Returns a list of positions and markers at the given column.
getColumn :: Int -> Board -> [(Position, Maybe Player)]
getColumn n (Board b) = filter ((n==) . fst . fst) $ A.assocs b

-- | Updates the board with the given push movement.
push :: Player -> Move -> Board -> Board
push pl mv (Board a) =
  let (selEls, initPos, mUpdate) = moveUpdate mv
      selected = takeWhile (isJust . snd) $ selEls (Board a)
      updates = filter (isValidPosition . fst) $ (initPos, Just pl) : map mUpdate selected
  in Board (a // updates)
    where
      moveUpdate (L n) = (getRow n, (1,n), \((x,y),p) -> ((x+1,y),p))
      moveUpdate (R n) = (reverse . getRow n, (boardHeight,n), \((x,y),p) -> ((x-1,y),p))
      moveUpdate (B n) = (reverse . getColumn n, (n,boardWidth), \((x,y),p) -> ((x,y-1),p))
      moveUpdate (T n) = (getColumn n, (n,1), \((x,y),p) -> ((x,y+1),p))

-- | Pushes a marker of the given player in the given direction (if valid).
makeMove :: Player -> Move -> Board -> Maybe Board
makeMove pl mov board
  | isValidMove mov = Just $ push pl mov board
  | otherwise       = Nothing

-- | Determines whether all position in a given list contain one marker.
fullLine :: [Maybe Player] -> Int
fullLine xs | all (== Just Nought) xs = 1
            | all (== Just Cross) xs = -1
            | otherwise = 0

-- | Counts all straights (positive number for Noughts, negative for Crosses).
scores :: Board -> Int
scores board =
  let rows = map (\x -> map snd $ getRow x board) [1..boardHeight]
      columns = map (\x -> map snd $ getColumn x board) [1..boardWidth]
  in sum $ map fullLine $ rows ++ columns

-- | Returns true if the given player has more straights on the board.
hasWon :: Player -> Board -> Bool
hasWon pl board =
  let score = scores board
  in case pl of Nought -> score > 0
                Cross -> score < 0

{- Strategies -}

-- | AI players are represented by a @Strategy@. The simple interface for @Strategy@ is a deterministic, stateless function
-- of the type @Player -> Board -> Position@; this function is given the information of what player it is and the current
-- board and then computes the next move.
--
-- The advanced interface for @Strategy@ is a probabilistic, stateful function. A Strategy must provide functions
--
-- @
-- initialState :: RandomGen g => g -> Player -> a
-- computeMove  :: RandomGen g => g -> a -> Player -> Board -> (Position, a)
-- @
--
-- A strategy maintains an internal state of arbitrary type @s@. @initialState@ must then return some value of
-- type @s@ which will be passed to the strategy for its first move. The @computeMove@ function again receives the
-- current board along with the current state of the strategy and must return the next move and the new state.
-- Both functions are given the information of what player they are playing as, and a fresh random generator with
-- every invocation in order to make random decisions.
--
-- Possible uses for the state are keeping a history of turns, or maintaining expensive data structures,
-- such as precomputed game trees or good moves.
data Strategy a = Strategy {initialState :: forall g. RandomGen g => g -> Player -> a,
                            computeMove  :: forall g. RandomGen g => g -> a -> Player -> Board -> (Move, a)}

-- | Lifts a given deterministic, stateless strategy to a ‘proper’ strategy
simpleStrategy :: (Player -> Board -> Move) -> Strategy ()
simpleStrategy s = Strategy (\ _ _ -> ()) (\_ _ pl f -> (s pl f, ()))
