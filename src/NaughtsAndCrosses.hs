module NaughtsAndCrosses
( GState
, GAction
, GID (..)
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

import Data.Maybe (isNothing)

data GID = X | O deriving (Show, Eq, Enum, Bounded, Read)
instance Id GID

data GState = GState { board :: Board, currPlayer :: GID } deriving (Eq)
instance TurnBased GID GState where
  turn = currPlayer

instance Show GState where
  show s = "Current Player: "++show (currPlayer s)++"\n"++board'
    where
      board' = show' a++"\n"++show' b++"\n"++show' c
      show' (x, y, z) = show x++" "++show y++" "++show z
      (Board (a, b, c)) = board s

instance State GState where
  start = GState { board = startBoard, currPlayer = X }
    where
    startBoard = Board (initTrip (initTrip (Space Nothing)))

  -- | Returns a finishMessage :: Maybe String (Nothing == not finished)
  finished state =
    case getWinner (board state) of
      (Just winner) -> Just (show state++"\nCongratulations player "++show winner)
      Nothing -> draw'
      where
        draw' | full (board state) = Just (show state++"\nGame drawn")
              | otherwise = Nothing

data GAction = GAction TripIndex TripIndex deriving (Show, Eq, Read, Bounded)
instance Enum GAction where
  fromEnum (GAction x y) = fromEnum x * 3 + fromEnum y
  toEnum n = GAction x y
    where
    x = toEnum (n `div` 3)
    y = toEnum (n `mod` 3)
instance Action GAction

instance MutableState GState GAction where
  update = updateBoard

instance Problem GState GAction

instance Game GState GID GAction where
  reward game player _
    | isNothing (getWinner (board game)) = 0.5
    | getWinner (board game) == Just player = 1
    | otherwise = 0

newtype Space = Space (Maybe GID) deriving Eq
instance Show Space where
  show (Space (Just X)) = "X"
  show (Space (Just O)) = "O"
  show (Space Nothing) = "_"

type Trip a = (a, a, a)
data TripIndex = A | B | C deriving (Show, Eq, Read, Enum, Bounded)

same :: Eq a => Trip a -> Bool
same (a, b, c) = (a == b) && (b == c) && (c == a)

newtype Board = Board (Trip (Trip Space)) deriving (Show, Eq)

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
updateBoard game (GAction x y) =
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

getWinner :: Board -> Maybe GID
getWinner board = foldr winner Nothing wins
  where
    wins = rows board++cols board++diags board
    winner :: Trip Space -> Maybe GID -> Maybe GID
    winner trip@(Space a, _, _) Nothing | same trip = a
    winner _ x = x
