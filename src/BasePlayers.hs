module BasePlayers
  ( playerUser
  , playerDoAny
  , playerDoAnyOpportune
  , playerReinforce
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)

import Game (Reward, gameLoop, gameReward, gameUpdate, Policy (Policy), Player (Player, policy, pid, update), Game, GameActors)


playerUser t =
  Player { policy = Policy userPlayer
         , pid = t
         , update = noupdate
         }
playerDoAny t =
  Player { policy = Policy doAny
         , pid = t
         , update = noupdate
         }
playerDoAnyOpportune t =
  Player { policy = Policy doAnyOpportune
         , pid = t
         , update = noupdate
         }

playerReinforce t =
  Player { policy = Policy greedyReinforcement
         , pid = t
         , update = updateGreedy
         }

promptFor :: Read a => String -> String -> IO a
promptFor prompt fail = do
  putStrLn prompt
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn fail >> promptFor prompt fail

userPlayer :: Game idT s a => idT -> s -> IO a
userPlayer pid state = do
  print state
  putStrLn $ "Reward: "++ show (gameReward state pid)
  promptFor "Play move: " "Couldn't parse input"

doAny :: Game idT s a => idT -> s -> IO a
doAny pid state = do
  let possibles = filter (canDo pid state) actions
  if null possibles
      then error ("FAILED TO FIND MOVE IN STATE: "++show state)
      else return $ head possibles

doAnyOpportune :: Game idT s a => idT -> s -> IO a
doAnyOpportune pid state = do
  let possibles = sortOn (calcReward pid state) (filter (canDo pid state) actions)
  if null possibles
    then error ("FAILED TO FIND MOVE IN STATE: "++show state)
    else return $ head possibles

greedyReinforcement :: Game idT s a => idT -> s -> IO a
greedyReinforcement  pid state = do
  let possibles = filter (canDo pid state) actions
  if null possibles
      then error ("FAILED TO FIND MOVE IN STATE: "++show state)
      else return $ head possibles

updateGreedy :: idT -> Player idT state action -> state -> Player idT state action
updateGreedy pid' player game = player

-- calcReward :: Game idT s a => idT s a -> Order
calcReward :: Game idT s a => idT -> s -> a -> Reward
calcReward pid state act = gameReward state' pid
  where
    state' = gameUpdate (pid, act) state


canDo :: Game idT s a => idT -> s -> a -> Bool
canDo pid state act = state /= gameUpdate (pid, act) state

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]

noupdate :: idT -> Player idT state action -> state -> Player idT state action
noupdate pid' player game = player
