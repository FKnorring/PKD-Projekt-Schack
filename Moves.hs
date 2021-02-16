module Moves where
import Board


rookmoves (x,y) clr brd = toFront (x,y) clr brd ++ toBack (x,y) clr brd ++ toLeft (x,y) clr brd ++ toRight (x,y) clr brd

toFront :: Coordinate -> PColor -> Board -> [Coordinate]
toFront (x,y) clr brd 
    | y-1 == -1 = []
    | isEmpty $ getSquare (x,y-1) brd = (x,y-1): toFront (x,y-1) clr brd 
    | getColor (getSquare (x,y-1) brd) == clr = []
    | otherwise = [(x,y-1)]

toBack :: Coordinate -> PColor -> Board -> [Coordinate]
toBack (x,y) clr brd 
    | y+1 == 8 = []
    | isEmpty $ getSquare (x,y+1) brd = (x,y+1): toBack (x,y+1) clr brd 
    | getColor (getSquare (x,y+1) brd) == clr = []
    | otherwise = [(x,y-1)]

toLeft :: Coordinate -> PColor -> Board -> [Coordinate]
toLeft (x,y) clr brd 
    | x-1 == -1 = []
    | isEmpty $ getSquare (x-1,y) brd = (x-1,y): toLeft (x-1,y) clr brd 
    | getColor (getSquare (x-1,y) brd) == clr = []
    | otherwise = [(x-1,y)]

toRight :: Coordinate -> PColor -> Board -> [Coordinate]
toRight (x,y) clr brd 
    | x+1 == 8 = []
    | isEmpty $ getSquare (x+1,y) brd = (x+1,y): toRight (x+1,y) clr brd 
    | getColor (getSquare (x+1,y) brd) == clr = []
    | otherwise = [(x+1,y)]

pawnMoves :: Coordinate -> PColor -> Board -> [Coordinate]
pawnMoves (x,6) White brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    [(x+1,5),(x-1,5)] ++ (x, 5) : ([(x, 4) | isEmpty (getSquare (x, 4) brd)])
pawnMoves (x,y) White brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    [(x+1,y-1),(x-1,y-1)] ++ [(x, y - 1) | isEmpty (getSquare (x, y - 1) brd)]
pawnMoves (x,1) Black brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    [(x+1,2),(x-1,2)] ++ (x, 2) : ([(x, 3) | isEmpty (getSquare (x, 3) brd)])
pawnMoves (x,y) Black brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    [(x+1,y+1),(x-1,y+1)] ++ [(x, y + 1) | isEmpty (getSquare (x, y + 1) brd)]
diagBR :: Coordinate -> PColor -> Board -> [Coordinate]
diagBR (x,y) clr brd 
    | x+1 == 8 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x+1,y+1) brd = (x+1,y+1): diagBR (x+1,y+1) clr brd 
    | getColor (getSquare (x+1,y+1) brd) == clr = []
    | otherwise = [(x+1,y+1)]

diagFR :: Coordinate -> PColor -> Board -> [Coordinate]
diagFR (x,y) clr brd 
    | x+1 == 8 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x+1,y-1) brd = (x+1,y-1): diagFR (x+1,y-1) clr brd 
    | getColor (getSquare (x+1,y-1) brd) == clr = []
    | otherwise = [(x+1,y-1)]

diagBL :: Coordinate -> PColor -> Board -> [Coordinate]
diagBL (x,y) clr brd 
    | x-1 == -1 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x-1,y+1) brd = (x-1,y+1): diagBL (x-1,y+1) clr brd 
    | getColor (getSquare (x-1,y+1) brd) == clr = []
    | otherwise = [(x-1,y+1)]

diagFL :: Coordinate -> PColor -> Board -> [Coordinate]
diagFL (x,y) clr brd 
    | x-1 == -1 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x-1,y-1) brd = (x-1,y-1): diagFL (x-1,y-1) clr brd 
    | getColor (getSquare (x-1,y-1) brd) == clr = []
    | otherwise = [(x-1,y-1)]


bishopmoves :: Coordinate -> PColor -> Board -> [Coordinate]
bishopmoves (x,y) clr brd = diagFR (x,y) clr brd ++ diagFL (x,y) clr brd ++ diagBR (x,y) clr brd ++ diagBL (x,y) clr brd

queenmoves :: Coordinate -> PColor -> Board -> [Coordinate]
queenmoves (x,y) clr brd = rookmoves (x,y) clr brd ++ bishopmoves (x,y) clr brd

kingmoves :: Coordinate -> PColor -> Board -> [Coordinate]
kingmoves (x,y) White brd = filter 
   (\x ->  (isEmpty (getSquare x brd)) || getColor (getSquare x brd) /= White) [(x+1,y+1),(x-1,y-1), (x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y),(x-1,y)]
kingmoves (x,y) Black brd = filter 
   (\x ->  (isEmpty (getSquare x brd)) || getColor (getSquare x brd) /= Black) [(x+1,y+1),(x-1,y-1), (x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y),(x-1,y)]
  