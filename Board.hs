module Board (Board, Square(..), PType(..), PColor(..), Coordinate, isEmpty, getColor, getSquare, other, getType) where

type Board = [[Square]]

{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Piece PColor PType deriving (Eq)

{- data Piece represents the specific piece-}

type Coordinate = (Int, Int)



{-data PType represents the different piece types-}

data PType = Bishop | Pawn | Rook | Knight | King | Queen deriving (Eq)


{- data PColor represent the pieces color-}
data PColor = White | Black | Null deriving (Eq, Show)

instance Show Square where
    show Empty = " "
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
getColor (Piece White _) = White
getColor (Piece Black _) = Black
getColor (Empty) = Null

getType :: Square -> PType
getType (Piece _ Pawn) = Pawn
getType (Piece _ Knight) = Knight
getType (Piece _ Bishop) = Bishop
getType (Piece _ Rook) = Rook
getType (Piece _ King) = King
getType (Piece _ Queen) = Queen

getSquare :: Coordinate -> Board -> Square
getSquare (x,y) board = (board !! y) !! x




other :: PColor -> PColor
other White = Black
other Black = White