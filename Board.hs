module Board where
import Debug.Trace


{-data Square represents if a certain square is empty or contains a piece-}
data Square = Empty | Piece PColor PType deriving (Eq)

{-type Board represents a chessboard containing squares
    INVARIANT: the board has to be a list containing 8 lists each conatining
    8 Square elements since the chess board is an 8x8 grid
-}
type Board = [[Square]]

{-type Coordinate represents a position on the board
    INVARIANT: the integers in the tuple can only be a number between 0 and 7
    any number outside of that will be a coordinate outside of the board
-}
type Coordinate = (Int, Int)



{-data PType represents the different piece types-}
data PType = Bishop | Pawn Move | Rook Moved | Knight | King Moved | Queen deriving (Eq)

{-data Move represents what type of move a pawn has performed-}
data Move = DoubleMove | SingleMove deriving (Eq)

{-data Moved represents if a piece has been moved from its starting position-}
data Moved = Moved | Unmoved deriving (Eq)

{- data PColor represent the pieces color-}
data PColor = White | Black | Null deriving (Eq, Show)


instance Show Square where
{-show square
    a function to retrieve the unicode character of the given piece at a square
    RETURNS: a string containing a unicode character corresponding to the piece at the square
    EXAMPLES: show (Piece White Bishop) == "♗"
              show (Piece Black (Rook _)) = "♜"
-}
    show Empty = " "
    show (Piece White (Pawn _)) = "♙"
    show (Piece White Bishop) = "♗"
    show (Piece White (Rook _)) = "♖"
    show (Piece White Knight) = "♘"
    show (Piece White (King _)) = "♔"
    show (Piece White Queen) = "♕"
    show (Piece Black (Pawn _)) = "♟"
    show (Piece Black Bishop) = "♝"
    show (Piece Black (Rook _)) = "♜"
    show (Piece Black Knight) = "♞"
    show (Piece Black (King _)) = "♚"
    show (Piece Black Queen) = "♛"
    
{-isEmpty square
    a function to check if a square is empty
    RETURNS: True if the square is empty, otherwise FALSE
    EXAMPLES: isEmpty Empty == True
              isEmpty (Piece White Bishop) == False
-}
isEmpty :: Square -> Bool 
isEmpty Empty = True
isEmpty _ = False

{-getColor square
    a function to retrive the color of a piece on a square
    RETURNS: White if the color of the piece is white
             Black if the color of the piece is black
             Null if the square is empty
    EXAMPLES: getColor (Piece Black Knight) == Black
              getColor Empty == Null
-}
getColor :: Square -> PColor
getColor (Piece White _) = White
getColor (Piece Black _) = Black
getColor Empty = Null

{-getType square
    a function to retrive the color of a piece on a square
    PRE: the square has to contain a piece
    RETURNS: the piece type on the given square
    EXAMPLES: getType (Piece White Pawn DoubleMove) == Pawn DoubleMove
              getType (Piece Black Rook Unmoved) == Rook Unmoved
-}
getType :: Square -> PType
getType (Piece _ (Pawn move)) = Pawn move
getType (Piece _ Knight) = Knight
getType (Piece _ Bishop) = Bishop
getType (Piece _ (Rook moved)) = Rook moved
getType (Piece _ (King moved)) = King moved
getType (Piece _ Queen) = Queen

{-getSquare coordinate board
    a function to retrieve the square given a coordinate on the board
    RETURNS: the square on the given coordinate on the given board
    EXAMPLES: getSquare (4,4) initBoard == Empty
              getSquare (7,4) initBoard == Piece White (King Unmoved)
-}
getSquare :: Coordinate -> Board -> Square
getSquare (x,y) board = (board !! y) !! x

{-other color
    a function to invert the given color
    PRE: color cannot be Null
    RETURNS: the opposite color
    EXAMPLES: other White = Black
              other Black = White
-}
other :: PColor -> PColor
other White = Black
other Black = White