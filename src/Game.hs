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

class (Show a, Read a, Bounded a, Enum a, Eq a) => Action a

instance (Enum a, Enum b, Bounded b) => Enum (a,b) where
  fromEnum (a, b) = a'*bRange+b'
    where
      a' = fromEnum a
      b' = fromEnum b
      bRange = fromEnum (maxBound::b) - fromEnum (minBound::b)
  toEnum c = (toEnum div', toEnum mod')
    where
      div' = c `div` bRange
      mod' = c `mod` bRange
      bRange = fromEnum (maxBound::b) - fromEnum (minBound::b)

class (Show state, Eq state) => State state where
  start :: state
  finished :: state -> Maybe String
  finished _ = Nothing

class (State state) => MutableState state updateT where
  update :: state -> updateT-> state
  update s _ = s

class (MutableState state action, Action action) => Problem state action where
  possible :: state -> action -> Bool
  possible state action = update state action /= state -- the default is that any thing that changes the world is legal

class (Show idT, Read idT, Bounded idT, Enum idT, Eq idT) => Id idT

class (MutableState agentState (problemState, Reward), Problem problemState action) => Agent problemState action agentState where
  policy :: agentState -> problemState -> IO action

class (Id idT) => TurnBased idT state where
  turn :: state -> idT

class (TurnBased idT pState, Problem pState action) => Game pState idT action where
  reward :: pState -> idT -> action -> Reward
  updateAgents :: (Agent pState action aState) => pState -> (idT -> aState) -> idT -> IO (pState, idT -> aState, action)
  updateAgents game players idT = do
      action <- policy agent game
      let game' = update game action
      let agent' = update agent (game', reward game' idT action)
      return (game', players' agent', action)
    where
      agent = players idT
      players' agent' idT'
        | idT' == idT = agent'
        | otherwise = players idT'

  gameLoop :: (Agent pState action aState) => pState -> (idT -> aState) -> IO (String, [action])
  gameLoop game players = do
    (game', players', action) <- updateAgents game players $ turn game
    case finished game' of
      Just output -> return (output, [])
      Nothing -> do
        (output, otherActions) <- gameLoop game' players'
        return (output, action:otherActions)


data GamePlayer game action = forall agent. (Agent game action agent) => GP agent

instance Show (GamePlayer g a) where
  show (GP ag) = show ag
instance Eq (GamePlayer g a) where
  (==) _ _ = False

instance State (GamePlayer game action) where
  start = error "Cannot default initialise a generic game player"

deriving instance MutableState (GamePlayer game action) (game, Reward)

instance (Problem game action) => Agent game action (GamePlayer game action) where
  policy (GP agent) = policy agent

