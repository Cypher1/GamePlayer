module TDLearner
  ( ReinforcePlayer
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

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

newtype ReinforcePlayer state = ReinforcePlayer (HashMap state Reward) deriving (Show, Eq)

instance (Show state, Eq state) => State (ReinforcePlayer state) where
  start = ReinforcePlayer HashMap.empty

instance (Show problem, Eq problem) => MutableState (ReinforcePlayer problem) (problem, Reward) where
  update (ReinforcePlayer map) (problem, reward) = ReinforcePlayer map -- TODO Update the map

instance (Problem problem action) => Agent problem action (ReinforcePlayer problem) where
  policy (ReinforcePlayer map) problem = do
    let possibles = filter (possible problem) actions -- TODO sort by values in map
    if null possibles
        then error ("FAILED TO FIND MOVE IN STATE: "++show problem)
        else return $ head possibles

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]
