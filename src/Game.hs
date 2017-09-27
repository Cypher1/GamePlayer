module Game
( gameLoop
, Reward
, Model (update, init)
, Player (policy)
, Game
, GameState
, GameActors
, GameAction
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
import Control.Applicative ((<*>), (<$>))

type Reward = Double

class (Show state, Eq state) => GameState state where
  gameInit :: state
  gameFinished :: state -> Maybe String

class (Bounded idT, Enum idT, Eq idT) => GameActors idT state where
  gameCanAct :: state -> [idT]
  gameReward :: state -> idT -> Reward

class (Show a, Read a, Bounded a, Enum a) => GameAction a

-- | s = state
-- | a = action
-- | idT = player ID
class (GameActors idT s, GameState s, GameAction a) => Game idT s a where
  gameUpdate :: (idT, a) -> s -> s

class Model model idT state where
  update :: model -> idT -> state -> model
  update model pid' game = model
  init :: idT -> model
  init id' = undefined

class (Game idT state action, Model model idT state) => Player model idT state action where
  policy :: model -> idT -> state -> IO action

updatePlayers :: (Model model idT state) => state -> (idT -> model) -> idT -> model
updatePlayers game players id = update (players id) id game

getAct :: Player model idT state action => state -> (idT -> model) -> action -> idT -> IO (idT, action)
getAct game players actT id' = (\act -> (id', act)) <$> policy (players id') id' game

gameLoop :: (Player model idT state action) => state -> (idT -> model) -> action -> IO ()
gameLoop game players actT = do
  actions' <- mapM (getAct game players actT) $ gameCanAct game
  let game' = foldr gameUpdate game actions'
  case gameFinished game' of
    Just output -> putStrLn output
    Nothing -> gameLoop game' (updatePlayers game' players) actT
