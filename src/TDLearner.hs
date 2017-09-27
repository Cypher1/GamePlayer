module TDLearner
  ( ReinforcePlayer
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Game
  ( Reward
  , gameLoop
  , gameReward
  , gameUpdate
  , Model (update, init)
  , Player (policy)
  , Game
  , GameActors
  )

newtype ReinforcePlayer state = ReinforcePlayer (HashMap state Reward)

instance Model (ReinforcePlayer state) idT state where
  update = updateGreedy
  init id' = ReinforcePlayer HashMap.empty
instance (Game idT state action) => Player (ReinforcePlayer state) idT state action where
  policy = greedyReinforcement

greedyReinforcement :: Game idT s a => model -> idT -> s -> IO a
greedyReinforcement  model pid state = do
  let possibles = filter (canDo pid state) actions
  if null possibles
      then error ("FAILED TO FIND MOVE IN STATE: "++show state)
      else return $ head possibles

updateGreedy :: ReinforcePlayer state -> idT -> state -> ReinforcePlayer state
updateGreedy model pid' game = model

canDo :: Game idT s a => idT -> s -> a -> Bool
canDo pid state act = state /= gameUpdate (pid, act) state

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]

