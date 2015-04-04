{-|
Module      : GameUtils
Description : Utilities for playing games.
Maintainer  : Tomas Tauber

This module provides some utilities for playing games.
(Adapted from original code for Othello by Manuel Eberl.)
-}

{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module GameUtils (
    playGame,  playGame', playGameInteractive, playGameInteractive'
  ) where

import Game
import System.Random
import Control.Arrow

{- Executing games -}

-- | Lifts a strategy to IO.
computeMoveIO :: Strategy a -> a -> Player -> Board -> IO (Move, a)
computeMoveIO strat s pl board = newStdGen >>= (\g -> return (computeMove strat g s pl board))

-- | Lifts a strategy's initial state function to IO
initialStateIO :: Strategy a -> Player -> IO a
initialStateIO strat pl = newStdGen >>= (\g -> return (initialState strat g pl))

-- | Given two IO strategies, lets them play against another and returns the result of the game.
playGameIO :: forall a b. (a -> Player -> Board -> IO (Move, a), b -> Player -> Board -> IO (Move, b)) ->
                              (a, b) -> IO ([Board], Player)
playGameIO strats initial = playGameIO' initial Cross [] emptyBoard
  where strat st pl board = case pl of
                              Cross  -> fmap (second (,snd st)) $ fst strats (fst st) pl board
                              Nought -> fmap (second (fst st,)) $ snd strats (snd st) pl board
        playGameIO' :: (a,b) -> Player -> [Board] -> Board -> IO ([Board], Player)
        playGameIO' st pl prevBoards board
          | hasWon (otherPlayer pl) board = return ([board], otherPlayer pl)
          | board `elem` prevBoards = return ([board], pl) -- repeats configuration
          | otherwise =
              do (pos, st') <- strat st pl board
                 case makeMove pl pos board of
                    Nothing -> error ("Invalid move by player " ++ show pl ++ ": " ++ show pos)
                    Just board' -> fmap (first (board:)) (playGameIO' st' (otherPlayer pl) (board : prevBoards) board')


-- | Lets two strategies play against another and returns the result.
playGame' ::(Strategy a, Strategy b) -> IO ([Board], Player)
playGame' (s1, s2) =
  do s1' <- initialStateIO s1 Cross
     s2' <- initialStateIO s2 Nought
     playGameIO (computeMoveIO s1, computeMoveIO s2) (s1', s2')

-- | Launches an interactive game against the given strategy on the console. The computer plays the given color.
playGame :: (Strategy a, Strategy b) -> IO ()
playGame strats =
  do (boards, pl) <- playGame' strats
     mapM_ (\x -> putStrLn (show x ++ "\n")) boards
     putStrLn ("Winner: " ++ show pl)

-- | Launches an interactive game against the given strategy, where the computer plays the given color,
--   and return the result.
playGameInteractive' :: Strategy a -> Player -> IO ([Board], Player)
playGameInteractive' strat pl =
       do initial <- initialStateIO strat pl
          case pl of
            Cross  -> playGameIO (computeMoveIO strat, prompt) (initial, ())
            Nought -> playGameIO (prompt, computeMoveIO strat) ((), initial)
          where readMove board =
                  do putStrLn "Please enter your move."
                     s <- fmap reads getLine :: IO [(Move, String)]
                     case s of
                       [(m, "")] | isValidMove m -> return m
                       _ -> putStrLn "Invalid move." >> readMove board
                prompt () pl board =
                  do putStrLn ('\n' : show board)
                     pos <- readMove board
                     return (pos, ())

-- | Launches an interactive game against a strategy on the console.
playGameInteractive :: Strategy a -> Player -> IO ()
playGameInteractive strat pl =
  do (boards, pl') <- playGameInteractive' strat pl
     putStrLn ('\n' : show (last boards))
     putStrLn (if pl == pl' then "Computer wins." else "You win.")
