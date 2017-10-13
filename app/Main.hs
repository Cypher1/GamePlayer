module Main where

import Game
import NaughtsAndCrosses
import BasePlayers
import TDLearner

import Text.Read (readMaybe)

players :: GID -> GamePlayer GState GAction
players t
  | t == minBound = GP (start :: DoAnyOpportunePlayer)
  | otherwise = GP (start :: ReinforcePlayer GState)

main :: IO ()
main = do
  putStrLn "Playing..."
  (output, moves::[GAction]) <- gameLoop (start::GState) players
  -- mapM_ print moves
  putStrLn output
  return ()
