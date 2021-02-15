import Data.Char

type Board = [[Square]]

data Square = Empty | Piece

data Piece = Piece PColor PType

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

hej = "hej"
 

