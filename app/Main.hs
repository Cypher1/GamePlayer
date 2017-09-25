module Main where

import Game (gameLoop, Policy (Policy), Player (Player, policy, pid), Game)
import NaughtsAndCrosses
import BasePlayers

import Text.Read (readMaybe)

players :: (Bounded idT, Eq idT, Game idT State Action) => idT -> Player idT State Action
players t
  | t == minBound = playerDoAny t
  | otherwise = playerUser t

main :: IO ()
main = gameLoop gameInit (players :: PlayerID -> Player PlayerID State Action)
