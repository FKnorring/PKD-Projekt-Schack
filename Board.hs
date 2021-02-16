module Board (Board, Square(..), Piece (..), PType(..), PColor(..), Coordinate, isEmpty, getColor, getSquare) where

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