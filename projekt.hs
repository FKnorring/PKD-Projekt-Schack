import Data.Char

type Board = [[Square]]

type Coordinate = (Int, Int)

data Square = Empty | Piece

data Piece = PColor PType

{-data PType represents the different piece types-}
data PType = Bishop | Pawn | Rook | Knight | King | Queen

{- data PColor represent the pieces color-}
data PColor = White | Black

{-data Square represents if a certain square is empty or contains a piece-}


{- data Piece represents the specific piece-}


showPiece :: PColor -> PType -> String 
showPiece White Pawn = "♙"
showPiece White Bishop = "♗"
showPiece White Rook = "♖"
showPiece White Knight = "♘"
showPiece White King = "♔"
showPiece White Queen = "♕"
showPiece Black Pawn = "♟"
showPiece Black Bishop = "♝"
showPiece Black Rook = "♜"
showPiece Black Knight = "♞"
showPiece Black King = "♚"
showPiece Black Queen = "♛"

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



