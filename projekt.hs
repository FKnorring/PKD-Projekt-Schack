import Data.Char

type Board = [[Square]]


{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Occupied Piece

{- data Piece represents the specific piece-}
data Piece = Piece PColor PType

type Coordinate = (Int, Int)




{-data PType represents the different piece types-}
data PType = Bishop | Pawn | Rook | Knight | King | Queen

{- data PColor represent the pieces color-}
data PColor = White | Black






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
coordinateToString (0,z) = 'a' : (show (z+1))
coordinateToString (1,z) = 'b' : (show (z+1))
coordinateToString (2,z) = 'c' : (show (z+1))
coordinateToString (3,z) = 'd' : (show (z+1))
coordinateToString (4,z) = 'e' : (show (z+1))
coordinateToString (5,z) = 'f' : (show (z+1))
coordinateToString (6,z) = 'g' : (show (z+1))
coordinateToString (7,z) = 'h' : (show (z+1))



