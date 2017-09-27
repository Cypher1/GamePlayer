module BasePlayers
  ( UserPlayer
  , DoAnyPlayer
  , DoAnyOpportunePlayer
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)

import Game
  ( Reward
  , gameLoop
  , gameReward
  , gameUpdate
  , Player (policy)
  , Model (update, init)
  , Game
  , GameActors
  )

data UserPlayer = UserPlayer

instance Model UserPlayer idT state where
instance (Game idT state action) => Player UserPlayer idT state action where
  policy = userPlayer

userPlayer :: Game idT s a => model -> idT -> s -> IO a
userPlayer _model pid state = do
  print state
  putStrLn $ "Reward: "++ show (gameReward state pid)
  promptFor "Play move: " "Couldn't parse input"


data DoAnyPlayer = DoAnyPlayer

instance Model DoAnyPlayer idT state where
instance (Game idT state action) => Player DoAnyPlayer idT state action where
  policy = doAny

doAny _model pid state = do
  let possibles = filter (canDo pid state) actions
  if null possibles
      then error ("FAILED TO FIND MOVE IN STATE: "++show state)
      else return $ head possibles


data DoAnyOpportunePlayer = DoAnyOpportunePlayer

instance Model DoAnyOpportunePlayer idT state where
instance (Game idT state action) => Player DoAnyOpportunePlayer idT state action where
  policy = doAnyOpportune


doAnyOpportune :: Game idT s a => model -> idT -> s -> IO a
doAnyOpportune _model pid state = do
  let possibles = sortOn (calcReward pid state) (filter (canDo pid state) actions)
  if null possibles
    then error ("FAILED TO FIND MOVE IN STATE: "++show state)
    else return $ head possibles

-- calcReward :: Game idT s a => idT s a -> Order
calcReward :: Game idT s a => idT -> s -> a -> Reward
calcReward pid state act = gameReward state' pid
  where
    state' = gameUpdate (pid, act) state


canDo :: Game idT s a => idT -> s -> a -> Bool
canDo pid state act = state /= gameUpdate (pid, act) state

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]

  -- update :: model -> idT -> state -> action -> model
  -- policy :: model -> idT -> state -> IO action
noupdate :: model -> idT -> state -> model
noupdate model pid' game = model

promptFor :: Read a => String -> String -> IO a
promptFor prompt fail = do
  putStrLn prompt
  line <- getLine
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn fail >> promptFor prompt fail

