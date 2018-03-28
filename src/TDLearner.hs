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

data ReinforcePlayer state idT action = ReinforcePlayer (Map state (Map action Reward)) Double deriving (Show, Eq)

instance (Bounded state, Enum state, Show state, Eq state, Show action, Eq action, Enum action, Bounded action, Ord state, Game state idT action) => State (ReinforcePlayer state idT action) where
  start = train start' 20
    where
      start' = ReinforcePlayer Map.empty 0
      train :: ReinforcePlayer state idT action -> Integer -> ReinforcePlayer state idT action
      train p 0 = p
      train p n = train p' (n-1)
        where
          p' = foldl trainOnProblem p $ filter (not.finished) [minBound..maxBound]
          trainOnProblem :: (Problem problem action, Bounded problem, Ord problem, Game problem idT action, Enum problem) => ReinforcePlayer problem idT action -> problem -> ReinforcePlayer problem idT action
          trainOnProblem agent problem = foldl (trainOnProblemAction problem) agent $ filter (possible problem) [minBound..maxBound]
          trainOnProblemAction :: (Problem problem action, Enum problem, Bounded problem, Ord problem, Game problem idT action) => problem -> ReinforcePlayer problem idT action -> action -> ReinforcePlayer problem idT action
          trainOnProblemAction problem agent action = update agent (problem, action, reward problem (turn problem::idT) action)

instance (Bounded problem, Enum problem, Ord problem, Ord action, Problem problem action, Action action, Game problem idT action) => MutableState (ReinforcePlayer problem idT action) (problem, action, Reward) where
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
        | null rewards = 0
        | otherwise = maximum rewards
        where
          rewards = map (getReward player problem') $ filter (possible problem') [minBound..maxBound]

instance (Bounded problem, Enum problem, Ord problem, Ord action, Problem problem action, Action action, Game problem idT action) => Agent problem action (ReinforcePlayer problem idT action) where
  -- policy :: agentState -> problemState -> IO action
  policy player@(ReinforcePlayer stateMap totalReward) problem =
    if null ordered
      then error ("FAILED TO FIND MOVE IN STATE (ReinforcePlayer): "++show problem)
      else do
        print $ map (\a -> (a, getReward player problem a)) ordered
        return $ head ordered
    where
      ordered :: [action]
      ordered = sortWith (getReward player problem) possibles
        where
          possibles :: [action]
          possibles = filter (possible problem) actions

getReward :: (Ord problem, Ord action, Problem problem action) => ReinforcePlayer problem idT action -> problem -> action -> Reward
getReward (ReinforcePlayer stateMap _) problem action = fromMaybe 1 $ Map.lookup action actionMap
  where
    actionMap = fromMaybe Map.empty $ Map.lookup problem stateMap -- TODO Store the default in the player as a parameter

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]
