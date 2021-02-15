import Data.Char

type Board = [[Square]]

{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Occupied Piece

{- data Piece represents the specific piece-}
data Piece = Piece PColor PType

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

hej = "hej"
 

