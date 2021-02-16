import Data.Char
import Control.Monad
import Data.List


type Board = [[Square]]


{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Occupied Piece

{- data Piece represents the specific piece-}
data Piece = Piece PColor PType

type Coordinate = (Int, Int)



{-data PType represents the different piece types-}
data PType = Bishop | Pawn | Rook | Knight | King | Queen

{- data PColor represent the pieces color-}
data PColor = White | Black deriving (Eq)

instance Show Square where
    show Empty = " "
    show (Occupied piece) = show piece

instance Show Piece where
    show (Piece White Pawn) = "♙"
    show (Piece White Bishop) = "♗"
    show (Piece White Rook) = "♖"
    show (Piece White Knight) = "♘"
    show (Piece White King) = "♔"
    show (Piece White Queen) = "♕"
    show (Piece Black Pawn) = "♟"
    show (Piece Black Bishop) = "♝"
    show (Piece Black Rook) = "♜"
    show (Piece Black Knight) = "♞"
    show (Piece Black King) = "♚"
    show (Piece Black Queen) = "♛"

isEmpty :: Square -> Bool 
isEmpty Empty = True
isEmpty _ = False

getColor :: Square -> PColor
getColor (Occupied (Piece White _)) = White
getColor (Occupied (Piece Black _)) = Black

getSquare :: Coordinate -> Board -> Square
getSquare (x,y) board = (board !! y) !! x

stringToCoordinate :: String -> Coordinate
stringToCoordinate "" = undefined
stringToCoordinate ('a':xs) = (0,(8 - read xs))
stringToCoordinate ('b':xs) = (1,(8 - read xs))
stringToCoordinate ('c':xs) = (2,(8 - read xs))
stringToCoordinate ('d':xs) = (3,(8 - read xs))
stringToCoordinate ('e':xs) = (4,(8 - read xs))
stringToCoordinate ('f':xs) = (5,(8 - read xs))
stringToCoordinate ('g':xs) = (6,(8 - read xs))
stringToCoordinate ('h':xs) = (7,(8 - read xs))




coordinateToString :: Coordinate -> String 
coordinateToString (0,z) = 'a' : (show (8 - z))
coordinateToString (1,z) = 'b' : (show (8 - z))
coordinateToString (2,z) = 'c' : (show (8 - z))
coordinateToString (3,z) = 'd' : (show (8 - z))
coordinateToString (4,z) = 'e' : (show (8 - z))
coordinateToString (5,z) = 'f' : (show (8 - z))
coordinateToString (6,z) = 'g' : (show (8 - z))
coordinateToString (7,z) = 'h' : (show (8 - z))

initBoard :: Board
initBoard = [[Occupied (Piece Black Rook),Occupied (Piece Black Knight),Occupied (Piece Black Bishop),Occupied (Piece Black Queen),Occupied (Piece Black King),Occupied (Piece Black Bishop),Occupied (Piece Black Knight),Occupied (Piece Black Rook)],
             [Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn)],
             [Occupied (Piece White Rook),Occupied (Piece White Knight),Occupied (Piece White Bishop),Occupied (Piece White Queen),Occupied (Piece White King),Occupied (Piece White Bishop),Occupied (Piece White Knight),Occupied (Piece White Rook)]]

printBoard :: Board -> IO ()
printBoard board = putStrLn $ printBoard' 1 8 board

printBoard' :: Int -> Int -> Board -> String 
printBoard' 1 8 ((a:xs):xss) = ("  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n8 ║"++show a++" ") ++ printBoard' 2 8 (xs:xss)
printBoard' 1 y ((a:xs):xss) = (show y++" ║"++show a++" ")                       ++ printBoard' 2 y (xs:xss)
printBoard' x y ((a:xs):xss) = ("║"++show a++" ")                                ++ printBoard' (x+1) y (xs:xss)
printBoard' _ 1 ([]:xs)      =  "║\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n"               ++ printBoard' 1 1 xs
printBoard' x y ([]:xs)      =  "║\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n"               ++ printBoard' 1 (y-1) xs
printBoard' x y []           =  "   a  b  c  d  e  f  g  h"

changeSquare :: Coordinate -> Board -> Square -> Board
changeSquare (x,0) (a:xs) square = changeSquare' x a square:xs
changeSquare (x,y) (a:xs) square = a : changeSquare (x,y-1) xs square

changeSquare' :: Int -> [Square] -> Square -> [Square]
changeSquare' 0 (a:xs) square = square:xs 
changeSquare' x (a:xs) square = a:changeSquare' (x-1) xs square

movePiece :: Board -> IO Board
movePiece board = do
    cord <- getLine
    let realCord = stringToCoordinate cord
        piece = getSquare realCord board
        newboard = changeSquare realCord board Empty
    newcord <- getLine
    let realNewCord = stringToCoordinate newcord
    return $ changeSquare realNewCord newboard piece

rookmoves :: Coordinate -> PColor -> Board -> [Coordinate]
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