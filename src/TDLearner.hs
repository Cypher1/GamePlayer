module TDLearner
  ( ReinforcePlayer
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Exts (sortWith)
import Numeric (fromRat)

import Game
  ( Reward
  , State (start, finished)
  , MutableState (update)
  , Action
  , Problem (possible)
  , Id
  , Agent (policy)
  , TurnBased (turn)
  , Game (reward)
  )

data ReinforcePlayer state action = ReinforcePlayer (Map state (Map action Reward)) Double deriving (Show, Eq)

instance (Show state, Eq state, Show action, Eq action) => State (ReinforcePlayer state action) where
  start = ReinforcePlayer Map.empty 0

instance (Ord problem, Ord action, Problem problem action, Action action) => MutableState (ReinforcePlayer problem action) (problem, action, Reward) where
  update player@(ReinforcePlayer states totalReward) (problem, action, reward) = ReinforcePlayer states' totalReward -- TODO Update the map
    where
      problem' = update problem action
      stateKey = problem
      actionKey = action -- should be able to hash / neural network this...

      states' = Map.insert stateKey actions' states
      actions = fromMaybe Map.empty $ Map.lookup stateKey states
      actions' = Map.insert actionKey reward' actions

      learningRate = 0.1
      discount = 0.9
      reward' = (1-learningRate) * getReward player problem action + learningRate* (reward +  discount * nextReward')
      nextReward' :: Reward
      nextReward'
        | null rewards = 0.5
        | otherwise = maximum rewards
        where
          rewards = map (getReward player problem') $ filter (possible problem') [minBound..maxBound]

instance (Ord problem, Ord action, Problem problem action) => Agent problem action (ReinforcePlayer problem action) where
  -- policy :: agentState -> problemState -> IO action
  policy player@(ReinforcePlayer stateMap totalReward) problem =
    if null ordered
      then error ("FAILED TO FIND MOVE IN STATE (ReinforcePlayer): "++show problem)
      else return $ head ordered
    where
      ordered :: [action]
      ordered = sortWith (getReward player problem) possibles
        where
          possibles :: [action]
          possibles = filter (possible problem) actions

getReward :: (Ord problem, Ord action, Problem problem action) => ReinforcePlayer problem action -> problem -> action -> Reward
getReward (ReinforcePlayer stateMap _) problem action = fromMaybe 1 $ Map.lookup action actionMap
  where
    actionMap = fromMaybe Map.empty $ Map.lookup problem stateMap -- TODO Store the default in the player as a parameter

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]
