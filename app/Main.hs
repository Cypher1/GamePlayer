module Main where

import Game (gameLoop, Policy (Policy), Player (Player, policy, pid), Game)
import NaughtsAndCrosses (gameInit, gameUpdate, gameReward, State, Action, PlayerID (X, O))

import Text.Read (readMaybe)

promptFor :: Read a => String -> String -> IO a
promptFor prompt fail = do
  putStrLn prompt
  line <- getLine
  case readMaybe ("Action "++line) of
    Just x -> return x
    Nothing -> putStrLn fail >> promptFor prompt fail

userPlayer :: (Game PlayerID State Action) => PlayerID -> State -> IO Action
userPlayer pid state = do
  print state
  putStrLn $ "Reward: "++ show (gameReward state pid)
  promptFor "Play move: " "Couldn't parse input" :: IO Action

dfs :: Game PlayerID State Action => PlayerID -> State -> IO Action
dfs pid state = do
  let possibles = filter (\act -> state /= gameUpdate (pid, act) state) actions
  if null possibles
    then error "FAILED TO FIND MOVE"
    else return $ head possibles

actions :: [Action]
actions = [minBound..maxBound]

players :: PlayerID -> Player PlayerID State Action
players t@X = Player {policy = Policy userPlayer, pid = t}
players t@O = Player {policy = Policy dfs, pid = t}

main :: IO ()
main = gameLoop gameInit players
