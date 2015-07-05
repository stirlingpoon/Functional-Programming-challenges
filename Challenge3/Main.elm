module Main where

{-
    Conway's life of game!
    Uses the simple algorithm to update each cell with # of alive neighbours, and that the field is toroidal.
-}

import List
import Set
import Dict
import Maybe
import Time
import Signal
import Window
import Debug
import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)

type alias Coordinates = (Int, Int)
type alias Board = Set.Set Coordinates

{-
    Note: The board coordinates start at (0, 0) on the top left, as diagram:
    -> x [0 .. length - 1]
    |
    v
    y [0 .. length - 1]
-}

{- MODEL & RULES -}
-- Square board of 100 x 100
-- Note: Length must be a multiple of 2
boardLength : Int
boardLength = 100

-- Empty board with all cells dead
emptyBoard : Board
emptyBoard = Set.empty

-- Default board, taken from the infinite 5x5 pattern on Wikipedia
defaultBoard : Board
defaultBoard = Set.fromList [(47, 47), (47, 48), (47, 51), (48, 47), (48, 50), (49, 50), (49, 51), (50, 49), (51, 47), (51, 49), (51, 50), (51, 51)]

-- The coordinates of all 8 neighbours
neighbourCoordinates : Coordinates -> Board
neighbourCoordinates (x, y) =
    let xl = (x-1+boardLength) % boardLength
        xr = (x+1) % boardLength
        yu = (y-1+boardLength) % boardLength
        yd = (y+1) % boardLength
    in Set.fromList [
        (x, yu), -- Top
        (xl, y), -- Left
        (xr, y), -- Right
        (x, yd), -- Bottom
        (xl, yu), -- Top left
        (xr, yu), -- Top right
        (xl, yd), -- Bottom left
        (xr, yd) -- Bottom right               
    ]

-- All coordinates that are possible alive cells next state
candidateTiles : Board -> Dict.Dict Coordinates Int
candidateTiles board = 
    let inc x = Just <| 1 + Maybe.withDefault 0 x
        dictInc k dict = Dict.update k inc dict
    in Set.foldl (\c acc -> Set.foldl dictInc acc <| neighbourCoordinates c) Dict.empty board

{- STATE -}
-- Returns the next state of the board
evolveBoard : Board -> Board
evolveBoard board =
    let candidates = candidateTiles board
        isAlive k v = v == 3 || (v == 2 && Set.member k board)
    in Set.fromList <| Dict.keys <| Dict.filter isAlive candidates

{- RENDER -}
-- Creates a colored box at (x, y) with length l
box color l (x, y) = move (x, y) <| filled color <| square l

{-
    Returns the list of colored boxes corresponding to the board
    Coordinates adjusted to fit Elm's coordinate system
-}
boxes board boxLength =
    let makeBox (x, y) =
        let halfLength = toFloat boardLength / 2
            x' = boxLength * (toFloat x - halfLength)
            y' = boxLength * (negate (toFloat y) + halfLength)
        in box darkCharcoal boxLength (x', y')
    in List.map makeBox <| Set.toList board

-- Display the board in a window of (w, h)
display : (Int, Int) -> Board -> Element
display (w, h) board =
    let l = min w h
        boxLength = (toFloat l) / (toFloat boardLength)
    in collage l l <| boxes (Debug.watch "board" board) boxLength

-- Update the state every 100ms
delta : Signal Float
delta = Time.fps 10

boardState : Signal Board
boardState = Signal.foldp (\t board -> evolveBoard board) defaultBoard delta

-- Update state and show board every delta
main : Signal Element
main = Signal.map2 display Window.dimensions boardState