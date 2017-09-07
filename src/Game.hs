module Game
( gameLoop
, Policy (Policy)
, Player (Player, policy, pid)
, Game
, GameState
, GameActors
, gameInit
, gameFinished
, gameCanAct
, gameUpdate
, gameReward
)
where

import qualified Data.Vector.Sized as Vec
import Data.Vector.Sized (Vector)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (unless)

type Reward = Double

data PlayerID = X | O deriving (Show, Eq, Enum, Bounded)

class GameState state where
  gameInit :: state
  gameFinished :: state -> Maybe String

class GameActors idT state where
  gameCanAct :: state -> [idT]
  gameReward :: state -> idT -> Reward

class (GameActors idT state, Bounded idT, Enum idT, GameState state) => Game idT state action where
  gameUpdate :: (idT, action) -> state -> state

newtype Policy idT state action = Policy (idT -> state -> IO action)

data Player idT state action = Player {policy :: Policy idT state action, pid :: idT}

instance Show idT => Show (Player idT state action) where
  show player = "Player: " ++ show (pid player)

getAction :: Game idT state action => Player idT state action -> state -> IO action
getAction player = pol (pid player)
  where
    Policy pol = policy player

getAct :: Game idT s a => s -> Player idT s a -> IO (idT, a)
getAct game p = do
  act <- getAction p game
  return (pid p, act)

gameLoop :: (Game idT s a) => s -> (idT -> Player idT s a) -> IO ()
gameLoop game players = do
  actions' <- mapM (getAct game. players) $ gameCanAct game
  let game' = foldr gameUpdate game actions'
  case gameFinished game' of
    Just output -> putStrLn output
    Nothing -> gameLoop game' players
