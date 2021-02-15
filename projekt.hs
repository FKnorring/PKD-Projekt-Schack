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







showPiece :: Piece -> String 
showPiece (Piece White Pawn) = "♙"
showPiece (Piece White Bishop) = "♗"
showPiece (Piece White Rook) = "♖"
showPiece (Piece White Knight) = "♘"
showPiece (Piece White King) = "♔"
showPiece (Piece White Queen) = "♕"
showPiece (Piece Black Pawn) = "♟"
showPiece (Piece Black Bishop) = "♝"
showPiece (Piece Black Rook) = "♜"
showPiece (Piece Black Knight) = "♞"
showPiece (Piece Black King) = "♚"
showPiece (Piece Black Queen) = "♛"

 

