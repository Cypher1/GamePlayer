module NaughtsAndCrosses
( GState
, GAction
, GID (..)
, getWinner
) where

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

import Data.Maybe (isJust, isNothing)

data GID = X | O deriving (Show, Eq, Enum, Bounded, Read, Ord)
instance Id GID

data GState = GState { board :: Board, currPlayer :: GID } deriving (Eq, Ord, Bounded)
instance Enum GState where
  fromEnum b = fromEnum (board b, currPlayer b)
  toEnum n = GState { board = board', currPlayer = currPlayer'}
    where
      (board', currPlayer') = toEnum n
instance TurnBased GID GState where
  turn = currPlayer

instance Show GState where
  show s = "\nCurrent Player: "++show (currPlayer s)++"\n"++board'++"\n"++ show ( finished s)
    where
      board' = show' a++"\n"++show' b++"\n"++show' c
      show' (x, y, z) = show x++" "++show y++" "++show z
      (Board (a, b, c)) = board s

instance State GState where
  start = GState { board = startBoard, currPlayer = X }
    where
      startBoard = Board (initTrip (initTrip (Space Nothing)))
  finished state = isJust (getWinner state) || full (board state) -- either there is a winner, a draw or the game continues

data GAction = GAction TripIndex TripIndex GID deriving (Show, Eq, Read, Bounded, Ord)
instance Enum GAction where
  fromEnum (GAction x y idT) = fromEnum (idT, (x, y))
  toEnum n = GAction x y idT
    where
      (idT, (x, y)) = toEnum n

instance Action GAction

instance MutableState GState GAction where
  update = updateBoard

instance Problem GState GAction

instance Game GState GID GAction where
  reward game player _
    | isNothing (getWinner game) = 0
    | getWinner game == Just player = 1
    | otherwise = -1

newtype Space = Space (Maybe GID) deriving (Eq, Ord)

instance Enum Space where
  fromEnum (Space x) = fromEnum x
  toEnum n = Space (toEnum n)
instance Bounded Space where
  minBound = Space minBound
  maxBound = Space maxBound

instance (Enum a) => Enum (Maybe a) where
  fromEnum Nothing = 0
  fromEnum (Just id) = fromEnum id + 1
  toEnum 0 = Nothing
  toEnum n = Just $ toEnum (n-1)

instance (Bounded a) => Bounded (Maybe a) where
  minBound = Nothing
  maxBound = Just maxBound

instance Show Space where
  show (Space (Just X)) = "X"
  show (Space (Just O)) = "O"
  show (Space Nothing) = "_"

type Trip a = (a, a, a)

instance (Bounded a, Enum a) => Enum (Trip a) where
  fromEnum (a1, a2, a3) = fromEnum (a1, (a2, a3))
  toEnum n = (a1, a2, a3)
    where
      (a1, (a2, a3)) = toEnum n

data TripIndex = A | B | C deriving (Show, Eq, Read, Enum, Bounded, Ord)

same :: Eq a => Trip a -> Bool
same (a, b, c) = (a == b) && (b == c) && (c == a)

newtype Board = Board (Trip (Trip Space)) deriving (Show, Eq, Ord)
instance Enum Board where
  fromEnum (Board trip) = fromEnum trip
  toEnum n = Board (toEnum n)
instance Bounded Board where
  minBound = Board minBound
  maxBound = Board maxBound

rows :: Board -> [Trip Space]
rows (Board (a, b, c)) = [a,b,c]
cols :: Board -> [Trip Space]
cols (Board ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3))) = [(a1, b1, c1), (a2, b2, c2), (a3, b3, c3)]
diags :: Board -> [Trip Space]
diags (Board ((a1, _, a3), (_, b2, _), (c1, _, c3))) = [(a1, b2, c3), (a3, b2, c1)]

initTrip :: a -> Trip a
initTrip a = (a, a, a)

getTrip :: TripIndex -> Trip a -> a
getTrip p (a, b, c)
  | p == A = a
  | p == B = b
  | p == C = c

toList :: Trip a -> [a]
toList (a, b, c) = [a,b,c]

mapTrip :: (a->b) -> Trip a -> Trip b
mapTrip f (a, b, c) = (f a, f b, f c)

updateTrip :: TripIndex -> (a->a) -> Trip a -> Trip a
updateTrip p f (a, b, c)
  | p == A = (f a, b, c)
  | p == B = (a, f b, c)
  | p == C = (a, b, f c)

updateBoard :: GState -> GAction -> GState
updateBoard game (GAction x y idT)
  | idT /= currPlayer game = game -- error $ "Player "++show idT++" cannot play not, it is "++show (currPlayer game) ++ "'s turn."
  | otherwise =
  case getTrip y (getTrip x board') of
    Space Nothing -> GState { board = update x y (Space (Just (currPlayer game))), currPlayer = nextPlayer}
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

getWinner :: GState -> Maybe GID
getWinner game = foldr winner Nothing wins
  where
    board' = board game
    wins = rows board'++cols board'++diags board'
    winner :: Trip Space -> Maybe GID -> Maybe GID
    winner trip@(Space a, _, _) Nothing | same trip = a
    winner _ x = x
