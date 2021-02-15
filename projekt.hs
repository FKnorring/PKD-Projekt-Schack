
{-data PType represents the different piece types-}
data PType = Bishop | Pawn | Rook | Knight | King | Queen

{- data PColor represent the pieces color-}
data PColor = White | Black

{-data Square represents if a certain square is empty or contains a piece-}
data Square =  Empty | Piece


{- data Piece represents the specific piece-}


showPiece :: PColor -> PType -> String 
showPiece White Pawn = "wP"
showPiece White Bishop = "wB"
showPiece White Rook = "wR"
showPiece White Knight = "wKN"
showPiece White King = "wK"
showPiece White Queen = "wQ"
showPiece Black Pawn = "bP"
showPiece Black Bishop = "bB"
showPiece Black Rook = "bR"
showPiece Black Knight = "bKn"
showPiece Black King = "bK"
showPiece Black Queen = "bQ"

 

