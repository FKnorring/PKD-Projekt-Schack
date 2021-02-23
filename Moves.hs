module Moves where
import Board


validSquares :: [Coordinate] -> [Coordinate]
validSquares = filter (`elem` ([(x,y) | x <- [0..7], y <- [0..7]]))

pawnMoves :: Coordinate -> PColor -> Board -> [Coordinate]
pawnMoves (x,6) White brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    (pawnMoves' (x,6) White) ++ if isEmpty (getSquare (x, 5) brd) then (x,5) : ([(x, 4) | isEmpty (getSquare (x, 4) brd)]) else []
pawnMoves (x,y) White brd = enPassantSquare (x,y) White brd ++ filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    (pawnMoves' (x,y) White)  ++ [(x, y - 1) | isEmpty (getSquare (x, y - 1) brd)]
pawnMoves (x,1) Black brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    (pawnMoves' (x,1) Black) ++ if isEmpty (getSquare (x, 2) brd) then (x,2) : ([(x, 3) | isEmpty (getSquare (x, 3) brd)]) else []
pawnMoves (x,y) Black brd = enPassantSquare (x,y) Black brd ++ filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    (pawnMoves' (x,y) Black) ++ [(x, y + 1) | isEmpty (getSquare (x, y + 1) brd)] ++ enPassantSquare (x,y) Black brd

enPassantSquare :: Coordinate -> PColor -> Board -> [Coordinate]
enPassantSquare (7,y) White brd 
    | getSquare (6,y) brd == Piece Black (Pawn DoubleMove) = [(6,y-1)]
    | otherwise = []
enPassantSquare (0,y) White brd 
    | getSquare (1,y) brd == Piece Black (Pawn DoubleMove) = [(1,y-1)]
    | otherwise = []
enPassantSquare (x,y) White brd 
    | getSquare (x+1,y) brd == Piece Black (Pawn DoubleMove) = [(x+1,y-1)]
    | getSquare (x-1,y) brd == Piece Black (Pawn DoubleMove) = [(x-1,y-1)]
    | otherwise = []
enPassantSquare (7,y) Black brd 
    | getSquare (6,y) brd == Piece White (Pawn DoubleMove) = [(6,y+1)]
    | otherwise = []
enPassantSquare (0,y) Black brd 
    | getSquare (1,y) brd == Piece White (Pawn DoubleMove) = [(1,y+1)]
    | otherwise = []
enPassantSquare (x,y) Black brd 
    | getSquare (x+1,y) brd == Piece White (Pawn DoubleMove) = [(x+1,y+1)]
    | getSquare (x-1,y) brd == Piece White (Pawn DoubleMove) = [(x-1,y+1)]
    | otherwise = []

pawnMoves' :: Coordinate -> PColor -> [Coordinate]
pawnMoves' (x,y) White = validSquares [(x+1,y-1),(x-1,y-1)]
pawnMoves' (x,y) Black = validSquares [(x+1,y+1),(x-1,y+1)]

rookmoves :: Coordinate -> PColor -> Board -> [Coordinate]
rookmoves (x,y) clr brd = toFront (x,y) clr brd ++ toBack (x,y) clr brd ++ toLeft (x,y) clr brd ++ toRight (x,y) clr brd

{-toFront (x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move to infront of it including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toFront (5,5) White initBoard = [(5,4),(5,3),(5,2),(5,1)]
            toFront (5,5) Black initBoard = [(5,4),(5,3),(5,2)]
            toFront (0,1) Black initBoard = []
            toFront (0,1) White initBoard = [(0,0)]

-}
toFront :: Coordinate -> PColor -> Board -> [Coordinate]
toFront (x,y) clr brd 
    | y-1 == -1 = []
    | isEmpty $ getSquare (x,y-1) brd = (x,y-1): toFront (x,y-1) clr brd 
    | getColor (getSquare (x,y-1) brd) == clr = []
    | otherwise = [(x,y-1)]


{-toBack (x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move to back from it including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toBack (5,5) White initBoard = []
            toBack (5,5) Black initBoard = [(5,6)]
            toBack (4,4) Black initBoard = [(4,5),(4,6)]
            toBack (4,4) White initBoard = [(4,5)]
            
            -}
toBack :: Coordinate -> PColor -> Board -> [Coordinate]
toBack (x,y) clr brd 
    | y+1 == 8 = []
    | isEmpty $ getSquare (x,y+1) brd = (x,y+1): toBack (x,y+1) clr brd 
    | getColor (getSquare (x,y+1) brd) == clr = []
    | otherwise = [(x,y+1)]

{-toLeft (x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move left on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toLeft (5,4) White initBoard = [(4,4),(3,4),(2,4),(1,4),(0,4)]
            toLeft (5,4) Black initBoard = [(4,4),(3,4),(2,4),(1,4),(0,4)]
            toLeft (4,4) Black initBoard = [(3,4),(2,4),(1,4),(0,4)]
            toLeft (4,4) White initBoard = [(3,4),(2,4),(1,4),(0,4)]
            -}
toLeft :: Coordinate -> PColor -> Board -> [Coordinate]
toLeft (x,y) clr brd 
    | x-1 == -1 = []
    | isEmpty $ getSquare (x-1,y) brd = (x-1,y): toLeft (x-1,y) clr brd 
    | getColor (getSquare (x-1,y) brd) == clr = []
    | otherwise = [(x-1,y)]

{-toRight (x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move right on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toRight (5,4) White initBoard = [(6,4),(7,4)]
            toRight (5,4) Black initBoard = [(6,4),(7,4)]
            toRight (4,4) Black initBoard = [(5,4),(6,4),(7,4)]
            toRight (4,4) White initBoard = [(5,4),(6,4),(7,4)]
            -}
toRight :: Coordinate -> PColor -> Board -> [Coordinate]
toRight (x,y) clr brd 
    | x+1 == 8 = []
    | isEmpty $ getSquare (x+1,y) brd = (x+1,y): toRight (x+1,y) clr brd 
    | getColor (getSquare (x+1,y) brd) == clr = []
    | otherwise = [(x+1,y)]

{-diaBR(x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move diagonally back right on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarBR (5,4) White initBoard = [(6,5)]
            diarBR (5,4) Black initBoard = [(6,5),(7,6)]
            diarBR (4,4) Black initBoard = [(5,5),(6,6)]
            diarBR (4,4) White initBoard = [(5,5)]
            -}
diagBR :: Coordinate -> PColor -> Board -> [Coordinate]
diagBR (x,y) clr brd 
    | x+1 == 8 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x+1,y+1) brd = (x+1,y+1): diagBR (x+1,y+1) clr brd 
    | getColor (getSquare (x+1,y+1) brd) == clr = []
    | otherwise = [(x+1,y+1)]


{-diaFR(x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move diagonally front left on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarFR (5,4) White initBoard = [(6,3),(7,2)]
            diarFR (5,4) Black initBoard = [(6,3),(7,2)]
            diarFR (4,4) Black initBoard = [(5,3),(6,2)]
            diarFR (4,4) White initBoard = [(5,3),(6,2),(7,1)]
            -}
diagFR :: Coordinate -> PColor -> Board -> [Coordinate]
diagFR (x,y) clr brd 
    | x+1 == 8 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x+1,y-1) brd = (x+1,y-1): diagFR (x+1,y-1) clr brd 
    | getColor (getSquare (x+1,y-1) brd) == clr = []
    | otherwise = [(x+1,y-1)]


{-diaBL(x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move diagonally back left on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarBL (5,4) White initBoard = [(4,5)]
            diarBL (5,4) Black initBoard = [(4,5),(3,6)]
            diarBL (4,4) Black initBoard = [(3,5),(2,6)]
            diarBL (4,4) White initBoard = [(3,5)]
            -}
diagBL :: Coordinate -> PColor -> Board -> [Coordinate]
diagBL (x,y) clr brd 
    | x-1 == -1 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x-1,y+1) brd = (x-1,y+1): diagBL (x-1,y+1) clr brd 
    | getColor (getSquare (x-1,y+1) brd) == clr = []
    | otherwise = [(x-1,y+1)]

{-diaFL(x,y) clr brd
a function that takes the coordiantes, color of the piece and a board and returns a list of all possible coordinates the piece can move diagonally front left on the board including one square occupied by a diiferent color piece
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diaFL (5,4) White initBoard = [(4,3),(3,2),(2,1)]
            diarFL (5,4) Black initBoard = [(4,3),(3,2)]
            diarFL (4,4) Black initBoard = [(3,3),(2,2)]
            diarFL (4,4) White initBoard = [(3,3),(2,2),(1,1)]
            -}

diagFL :: Coordinate -> PColor -> Board -> [Coordinate]
diagFL (x,y) clr brd 
    | x-1 == -1 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x-1,y-1) brd = (x-1,y-1): diagFL (x-1,y-1) clr brd 
    | getColor (getSquare (x-1,y-1) brd) == clr = []
    | otherwise = [(x-1,y-1)]

{-
bishopmoves (x,y) clr brd
a function that checks all possible moves for the bishop and puts them in a list of coordinates
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS : A list of tuples containing two ints
  EXAMPLES: bishopmoves (4,4) White initBoard = [(5,3),(6,2),(7,1),(3,3),(2,2),(1,1),(5,5),(3,5)]
            bishopmoves (4,4) Black initBoard = [(5,3),(6,2),(3,3),(2,2),(5,5),(6,6),(3,5),(2,6)]
            bishopmoves (3,3) White initBoard = [(4,2),(5,1),(2,2),(1,1),(4,4),(5,5),(2,4),(1,5)]
            bishopmoves (3,3) Black initBoard = [(4,2),(2,2),(4,4),(5,5),(6,6),(2,4),(1,5),(0,6)]

-}
bishopmoves :: Coordinate -> PColor -> Board -> [Coordinate]
bishopmoves (x,y) clr brd = diagFR (x,y) clr brd ++ diagFL (x,y) clr brd ++ diagBR (x,y) clr brd ++ diagBL (x,y) clr brd

queenmoves :: Coordinate -> PColor -> Board -> [Coordinate]
queenmoves (x,y) clr brd = rookmoves (x,y) clr brd ++ bishopmoves (x,y) clr brd

kingmoves :: Coordinate -> PColor -> Board -> [Coordinate]
kingmoves (x,y) clr brd = filter 
   (\x ->  isEmpty (getSquare x brd) || getColor (getSquare x brd) /= clr) (kingmoves' (x,y))


kingmoves' :: Coordinate -> [Coordinate]
kingmoves' (x,y) = validSquares [(x+1,y+1),(x-1,y-1),(x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y),(x-1,y)]


horseMoves :: Coordinate -> PColor -> Board -> [Coordinate]
horseMoves (x,y) clr brd = filter 
   (\x ->  isEmpty (getSquare x brd) || getColor (getSquare x brd) /= clr) (horseMoves' (x,y))


horseMoves' :: Coordinate -> [Coordinate]
horseMoves' (x,y) = validSquares [(x+2,y+1),(x+2,y-1),(x-2,y+1), (x-2, y+1), (x+1,y+2) , (x+1,y-2) , (x-1,y+2), (x-1,y-2)]

--OpPieces :: Board -> [Coordinate]

getKing :: PColor -> Board -> Coordinate
getKing clr brd = head (filter (\x -> getSquare x brd == Piece clr (King Moved) || getSquare x brd == Piece clr (King Unmoved)) [(x,y) | x <- [0..7], y <- [0..7]])

possibleMoves :: PColor -> Board -> [Coordinate]
possibleMoves clr brd = concatMap (\x -> case getType (getSquare x brd) of 
        (Pawn _) -> pawnMoves x clr brd
        Knight -> horseMoves x clr brd
        Bishop -> bishopmoves x clr brd
        Queen -> queenmoves x clr brd
        (Rook _) -> rookmoves x clr brd
        (King _) -> kingmoves x clr brd)
        $ filter (\x -> getColor (getSquare x brd) == clr) [(x,y) | x <- [0..7], y <- [0..7]]

isChecked :: PColor -> Board -> Bool
isChecked clr brd = getKing clr brd `elem` possibleMoves (other clr) brd

getPromotedPawn :: PColor -> Board -> [Coordinate]
getPromotedPawn White brd = filter (\x -> getSquare x brd == Piece White (Pawn SingleMove)) [(x,y) | x <- [0..7], y <- [0]]
getPromotedPawn Black brd = filter (\x -> getSquare x brd == Piece Black (Pawn SingleMove)) [(x,y) | x <- [0..7], y <- [7]]

clearKSide' :: PColor ->  Board   -> [Square]
clearKSide' White brd = map (\x -> getSquare x brd) [(x,y) | x <- [4..7], y <- [7]] 
clearKSide' Black brd = map (\x -> getSquare x brd) [(x,y) | x <- [4..7], y <- [0]] 


clearKSide :: PColor -> Board -> Bool
clearKSide clr brd = (clearKSide' clr brd) == [(Piece clr (King Unmoved)),(Empty),(Empty),(Piece clr (Rook Unmoved))]



clearQSide' :: PColor ->  Board   -> [Square]
clearQSide' White brd = map (\x -> getSquare x brd) [(x,y) | x <- [0..4], y <- [7]] 
clearQSide' Black brd = map (\x -> getSquare x brd) [(x,y) | x <- [0..4], y <- [0]] 


clearQSide :: PColor -> Board -> Bool
clearQSide clr brd = (clearQSide' clr brd) == [(Piece clr (Rook Unmoved)),(Empty),(Empty),(Empty),(Piece clr (King Unmoved))]


castleBoard :: Board
castleBoard = [[Piece Black (Rook Unmoved),Empty,Empty,Empty,Piece Black (King Unmoved),Empty,Empty,Piece Black (Rook Unmoved)],
             [Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove)],
             [Piece White (Rook Unmoved),Empty,Empty,Empty,Piece White (King Unmoved),Empty,Empty,Piece White (Rook Unmoved)]]