module BasePlayers
  ( User
  , DoAnyPlayer
  , DoAnyOpportunePlayer
  ) where

import Text.Read (readMaybe)
import Data.List (sortOn)

import Game
  ( Reward
  , State (start, finished)
  , MutableState (update)
  , Action
  , Problem (possible)
  , Id
  , Agent (policy)
  , Game ()
  )

actions :: (Bounded a, Enum a) => [a]
actions = [minBound..maxBound]

promptFor :: Read a => String -> String -> IO a
promptFor prompt fail = do
  putStrLn prompt
  line' <- getLine
  let line = "GAction "++line'++" X"
  case readMaybe line of
    Just x -> return x
    Nothing -> putStrLn fail >> promptFor prompt fail


data User = User deriving (Show, Read, Eq)
instance State User where
  start = User
instance MutableState User (problem, action, Reward)
instance (Action action, Problem problem action) => Agent problem action User where
  policy _ problem = do
    print problem
    -- putStrLn $ "Reward: "++ show (reward problem action)
    promptFor "Play move: " "Couldn't parse input"

data DoAnyPlayer = DoAnyPlayer deriving (Show, Read, Eq)
instance State DoAnyPlayer where
  start = DoAnyPlayer
instance MutableState DoAnyPlayer (problem, action, Reward) where
instance (Problem problem action) => Agent problem action DoAnyPlayer where
  policy _ problem = do
    let possibles = filter (possible problem) actions
    if null possibles
       then error ("FAILED TO FIND MOVE IN STATE (DoAnyPlayer): "++show problem)
        else return $ head possibles

data DoAnyOpportunePlayer = DoAnyOpportunePlayer deriving (Show, Read, Eq)
instance State DoAnyOpportunePlayer where
  start = DoAnyOpportunePlayer
instance MutableState DoAnyOpportunePlayer (problem, action, Reward) where
instance (Problem problem action) => Agent problem action DoAnyOpportunePlayer where
  policy _ problem = do
    let possibles = filter (possible problem) actions
    if null possibles
        then error ("FAILED TO FIND MOVE IN STATE: "++show problem)
        else return $ head possibles
