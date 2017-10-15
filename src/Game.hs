module Game
( Reward
, State (start, finished)
, MutableState (update)
, Action
, Problem (possible)
, Id
, Agent (policy)
, TurnBased (turn)
, Game (reward, gameLoop)
, GamePlayer (GP)
)
where

type Reward = Double

class (Show a, Read a, Bounded a, Enum a, Eq a, Ord a) => Action a

instance (Enum a, Enum b, Bounded b) => Enum (a,b) where
  fromEnum (a, b) = a'*bRange+b'
    where
      a' = fromEnum a
      b' = fromEnum b
      bRange = fromEnum (maxBound::b) - fromEnum (minBound::b) + 1
  toEnum c = (toEnum div', toEnum mod')
    where
      div' = c `div` bRange
      mod' = c `mod` bRange
      bRange = fromEnum (maxBound::b) - fromEnum (minBound::b) + 1

class (Show state, Eq state) => State state where
  start :: state
  finished :: state -> Bool
  finished _ = False

class (State state) => MutableState state updateT where
  update :: state -> updateT-> state
  update s _ = s

class (MutableState state action, Action action) => Problem state action where
  possible :: state -> action -> Bool
  possible state action = update state action /= state -- the default is that any thing that changes the world is legal

class (Show idT, Read idT, Bounded idT, Enum idT, Eq idT) => Id idT

class (MutableState agentState (problemState, action, Reward), Problem problemState action) => Agent problemState action agentState where
  policy :: agentState -> problemState -> IO action

class (Id idT) => TurnBased idT state where
  turn :: state -> idT

class (TurnBased idT pState, Problem pState action) => Game pState idT action where
  reward :: pState -> idT -> action -> Reward
  updateAgents :: (Agent pState action aState) => pState -> (idT -> aState) -> idT -> IO (pState, idT -> aState, action)
  updateAgents game players idT = updateAgents' <$> policy agent game
    where
      agent = players idT
      updateAgents' action = (game', players', action)
        where
          game' = update game action
          players' idT'
            | idT' == idT = update agent (game, action, reward game' idT action)
            | otherwise = players idT'

  gameLoop :: (Agent pState action aState) => pState -> (idT -> aState) -> IO (pState, idT -> aState, [action])
  gameLoop game players = if finished game
      then return (game, players, [])
      else do
        (game', players', action) <- updateAgents game players $ turn game
        (\(g, p, otherActions) -> (g, p, action:otherActions)) <$> gameLoop game' players'


data GamePlayer game action = forall agent. (Agent game action agent) => GP agent

instance Show (GamePlayer g a) where
  show (GP ag) = show ag
instance Eq (GamePlayer g a) where
  (==) _ _ = False

instance State (GamePlayer game action) where
  start = error "Cannot default initialise a generic game player"

instance MutableState (GamePlayer game action) (game, action, Reward) where
  update (GP agent) = GP . update agent

instance (Problem game action) => Agent game action (GamePlayer game action) where
  policy (GP agent) = policy agent

