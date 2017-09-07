module NaughtsAndCrosses
( gameInit
, gameReward
, gameUpdate
, State
, Action
, PlayerID (..)
) where

import Game
  ( gameLoop
  , Game
  , GameState
  , GameActors
  , gameUpdate
  , gameReward
  , gameInit
  , gameFinished
  , gameCanAct
  )

import Data.Maybe (isJust, isNothing)

data PlayerID = X | O deriving (Show, Eq, Enum, Bounded)

newtype Space = Space (Maybe PlayerID) deriving Eq

instance Show Space where
  show (Space (Just X)) = "X"
  show (Space (Just O)) = "O"
  show (Space Nothing) = "_"

data Trip a = Trip a a a deriving (Show, Eq)
data TripIndex = A | B | C deriving (Show, Eq, Read, Enum, Bounded)

same :: Eq a => Trip a -> Bool
same (Trip a b c) = (a == b) && (b == c) && (c == a)

newtype Board = Board (Trip (Trip Space)) deriving (Show, Eq)

rows :: Board -> [Trip Space]
rows (Board (Trip a b c)) = [a,b,c]
cols :: Board -> [Trip Space]
cols (Board (Trip (Trip a1 a2 a3) (Trip b1 b2 b3) (Trip c1 c2 c3))) = [Trip a1 b1 c1, Trip a2 b2 c2, Trip a3 b3 c3]
diags :: Board -> [Trip Space]
diags (Board (Trip (Trip a1 _ a3) (Trip _ b2 _) (Trip c1 _ c3))) = [Trip a1 b2 c3, Trip a3 b2 c1]

initTrip :: a -> Trip a
initTrip a = Trip a a a

data State = State { board :: Board, currPlayer :: PlayerID } deriving (Eq)

instance Show State where
  show s = "Current Player: "++show (currPlayer s)++"\n"++board'
    where
      board' = show' a++"\n"++show' b++"\n"++show' c
      show' (Trip x y z) = show x++" "++show y++" "++show z
      (Board (Trip a b c)) = board s

data Action = Action TripIndex TripIndex deriving (Show, Eq, Read, Bounded)

instance Enum Action where
  fromEnum (Action x y) = fromEnum x * 3 + fromEnum y
  toEnum n = Action x y
    where
    x = toEnum (n `div` 3)
    y = toEnum (n `mod` 3)

instance GameState State where
  gameInit = State { board = startBoard, currPlayer = X }
    where
    startBoard = Board (initTrip (initTrip (Space Nothing)))
  gameFinished state =
    case getWinner (board state) of
      (Just winner) -> Just (show state++"\nCongratulations player "++show winner)
      Nothing -> draw'
      where
        draw' | full (board state) = Just (show state++"\nGame drawn")
              | otherwise = Nothing

instance GameActors PlayerID State where
  gameCanAct state = [currPlayer state]
  gameReward game player
    | isNothing (getWinner (board game)) = 0.5
    | getWinner (board game) == Just player = 1
    | otherwise = 0

instance Game PlayerID State Action where
  -- gameUpdate :: (PlayerID, action) -> state -> Maybe state
  gameUpdate (player, action) game
    | player == currPlayer game = updateBoard game (player, action)
    | otherwise = game
  -- gameReward :: state -> PlayerID -> Reward

getTrip :: TripIndex -> Trip a -> a
getTrip p (Trip a b c)
  | p == A = a
  | p == B = b
  | p == C = c

toList :: Trip a -> [a]
toList (Trip a b c) = [a,b,c]

mapTrip :: (a->b) -> Trip a -> Trip b
mapTrip f (Trip a b c) = Trip (f a) (f b) (f c)

updateTrip :: TripIndex -> (a->a) -> Trip a -> Trip a
updateTrip p f (Trip a b c)
  | p == A = Trip (f a) b c
  | p == B = Trip a (f b) c
  | p == C = Trip a b (f c)

updateBoard :: State -> (PlayerID, Action) -> State
updateBoard game (player, Action x y) =
  case getTrip y (getTrip x board') of
    Space Nothing -> State { board = update x y (Space (Just player)), currPlayer = nextPlayer}
    _ -> game
  where
    Board board' = board game
    update :: TripIndex -> TripIndex -> Space -> Board
    update x y v = Board (updateTrip x (updateTrip y (const v)) board')
    nextPlayer = next (currPlayer game)
      where
        next X = O
        next O = X

full :: Board -> Bool
full (Board b) = all (notElem (Space Nothing) . toList) $ toList b

getWinner :: Board -> Maybe PlayerID
getWinner board = foldr winner Nothing wins
  where
    wins = rows board++cols board++diags board
    winner :: Trip Space -> Maybe PlayerID -> Maybe PlayerID
    winner trip@(Trip (Space a) _ _) Nothing | same trip = a
    winner _ x = x
