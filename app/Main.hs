module Main where

import Game
  ( gameLoop
  , Model (update, init)
  , Player (policy)
  , Game
  )
import NaughtsAndCrosses
import BasePlayers
import TDLearner

import Text.Read (readMaybe)

data IorS = I Int | S String

instance Show IorS where
  show (I i) = show i
  show (S s) = show s

data PlayerSet state = DoAny DoAnyPlayer | Reinforce (ReinforcePlayer state)

instance Model (PlayerSet state) PlayerID state
  where
    update (DoAny m) id' state = DoAny $ Game.update m id' state
    update (Reinforce m) id' state = Reinforce $ Game.update m id' state
    init t
      | t == minBound = DoAny $ Game.init t
      | otherwise = Reinforce $ Game.init t

instance (Game idT state action, Model (PlayerSet state) idT state) => Player (PlayerSet state) idT state action
  where
    policy (DoAny m) = policy m
    policy (Reinforce m) = policy m

players :: Action -> PlayerID -> PlayerSet State
players action = Game.init

main :: IO ()
main = gameLoop (gameInit::State) ((players::Action -> PlayerID -> PlayerSet State) undefined) (undefined::Action)
