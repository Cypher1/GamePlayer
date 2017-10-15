module Main where

import Game
import NaughtsAndCrosses
import BasePlayers
import TDLearner

import Text.Read (readMaybe)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

players :: GID -> GamePlayer GState GAction
players t
  | t == minBound = GP (start :: User) -- ReinforcePlayer GState GAction)
  | otherwise = GP (start :: ReinforcePlayer GState GAction)

main :: IO ()
main = do
  --players' <- runGame players 10000
  putStrLn "Time to play"
  --let playersWithUser idT = if idT == minBound
   --                           then GP (start :: User)
    --                          else players' idT
  players'' <- runGame players 100 -- WithUser 100
  return ()

runGame :: (GID -> GamePlayer GState GAction) -> Integer -> IO (GID -> GamePlayer GState GAction)
runGame players 0 = return players
runGame players plays = do
      putStrLn "Playing..."
      (game, players', moves::[GAction]) <- gameLoop (start::GState) players
      -- mapM_ (print.players') ([minBound..maxBound]::[GID])
      -- mapM_ print moves
      putStrLn $ fromMaybe "DRAW" $ (\idT -> "WINNER: "++show idT++"\n") <$> getWinner game
      runGame players' (plays-1)
