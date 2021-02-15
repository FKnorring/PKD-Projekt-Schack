import Data.Char
import Control.Monad
import Data.List

type Board = [[Square]]

{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Occupied Piece

{- data Piece represents the specific piece-}
data Piece = Piece PColor PType

{-data PType represents the different piece types-}
data PType = Bishop | Pawn | Rook | Knight | King | Queen

{- data PColor represent the pieces color-}
data PColor = White | Black

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

initBoard :: Board
initBoard = [[Occupied (Piece Black Rook),Empty,Empty,Empty,Empty,Empty,Empty,Occupied (Piece Black Rook)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Occupied (Piece White Rook),Empty,Empty,Empty,Empty,Empty,Empty,Occupied (Piece White Rook)]]

printBoard :: Board -> String 
printBoard ((x:xs):xss) = ("|"++show x) ++ printBoard (xs:xss)
printBoard ([]:xs) = "|\n" ++ printBoard xs
printBoard [] = ""

 

