module Moves where
import Board

{-validSquares
    a function that filters a list of coordinates with all the possible coordinates on the board
-}
validSquares :: [Coordinate] -> [Coordinate]
validSquares = filter (`elem` ([(x,y) | x <- [0..7], y <- [0..7]]))


{-pawnMoves coordinate color board
  a function that checks all possible moves for a white or black pawn and puts them in a list of coordinates
  RETURNS: a list of coordinates, the possible moves for a pawn in coordinate of color
  EXAMPLES: pawnMoves (2,2) White initBoard = [(3,1),(1,1)]
            pawnMoves (2,1) Black initBoard = [(2,2),(2,3)]
            pawnMoves (4,4) White initBoard = [(4,3)]
            pawnMoves (2,3) Black initBoard = [(2,4)]
 -}
pawnMoves :: Coordinate -> PColor -> Board -> [Coordinate]
pawnMoves (x,6) White brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    (pawnCaptures (x,6) White) ++ if isEmpty (getSquare (x, 5) brd) then (x,5) : ([(x, 4) | isEmpty (getSquare (x, 4) brd)]) else []
pawnMoves (x,y) White brd = enPassantSquare (x,y) White brd ++ filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= White) 
    (pawnCaptures (x,y) White)  ++ [(x, y - 1) | isEmpty (getSquare (x, y - 1) brd)]
pawnMoves (x,1) Black brd = filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    (pawnCaptures (x,1) Black) ++ if isEmpty (getSquare (x, 2) brd) then (x,2) : ([(x, 3) | isEmpty (getSquare (x, 3) brd)]) else []
pawnMoves (x,y) Black brd = enPassantSquare (x,y) Black brd ++ filter 
    (\x -> not (isEmpty (getSquare x brd)) && getColor (getSquare x brd) /= Black) 
    (pawnCaptures (x,y) Black) ++ [(x, y + 1) | isEmpty (getSquare (x, y + 1) brd)] ++ enPassantSquare (x,y) Black brd


{-enPassantSquare coordinate color board
  a function that gives the coordinate of the square for an en passant move if such is availible
  RETURNS: a list containing either nothing or the coordinate containing the en passant move
  EXAMPLES: enPassantSquare (6,4) White initBoard == []
 -}
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

{-pawnCaptures crd clr 
    a function that returns the coordinates where a clr pawn on crd can capture a piece if there are any
    RETURNS: a list containing tuples of two ints.
    EXAMPLES: pawnCaptures (7,6) White = [(6,5)]
              pawnCaptures (6,6) White = [(7,5),(5,5)]
              pawnCaptures (0,1) Black = [(1,2)]
              pawnCaptures (1,1) Black = [(2,2),(0,2)]
               -}
pawnCaptures :: Coordinate -> PColor -> [Coordinate]
pawnCaptures (x,y) White = validSquares [(x+1,y-1),(x-1,y-1)]
pawnCaptures (x,y) Black = validSquares [(x+1,y+1),(x-1,y+1)]



{-toFront (x,y) clr brd
    a function that finds all legal coordinates in front of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toFront (5,5) White initBoard = [(5,4),(5,3),(5,2),(5,1)]
            toFront (5,5) Black initBoard = [(5,4),(5,3),(5,2)]
            toFront (0,1) Black initBoard = []
            toFront (0,1) White initBoard = [(0,0)]

-}
toFront :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: y
toFront (x,y) clr brd 
    | y-1 == -1 = []
    | isEmpty $ getSquare (x,y-1) brd = (x,y-1): toFront (x,y-1) clr brd 
    | getColor (getSquare (x,y-1) brd) == clr = []
    | otherwise = [(x,y-1)]


{-toBack (x,y) clr brd
    a function that finds all legal coordinates behind (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toBack (5,5) White initBoard = []
            toBack (5,5) Black initBoard = [(5,6)]
            toBack (4,4) Black initBoard = [(4,5),(4,6)]
            toBack (4,4) White initBoard = [(4,5)]
            
            -}
toBack :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: 8-y
toBack (x,y) clr brd 
    | y+1 == 8 = []
    | isEmpty $ getSquare (x,y+1) brd = (x,y+1): toBack (x,y+1) clr brd 
    | getColor (getSquare (x,y+1) brd) == clr = []
    | otherwise = [(x,y+1)]

{-toLeft (x,y) clr brd
    a function that finds all legal coordinates to the left of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toLeft (5,4) White initBoard = [(4,4),(3,4),(2,4),(1,4),(0,4)]
            toLeft (5,4) Black initBoard = [(4,4),(3,4),(2,4),(1,4),(0,4)]
            toLeft (4,4) Black initBoard = [(3,4),(2,4),(1,4),(0,4)]
            toLeft (4,4) White initBoard = [(3,4),(2,4),(1,4),(0,4)]
            -}
toLeft :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: x
toLeft (x,y) clr brd 
    | x-1 == -1 = []
    | isEmpty $ getSquare (x-1,y) brd = (x-1,y): toLeft (x-1,y) clr brd 
    | getColor (getSquare (x-1,y) brd) == clr = []
    | otherwise = [(x-1,y)]

{-toRight (x,y) clr brd
    a function that finds all legal coordinates to the right of (x,y) for clr on brd
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS: a list of tuples containing two ints
  EXAMPLES: toRight (5,4) White initBoard = [(6,4),(7,4)]
            toRight (5,4) Black initBoard = [(6,4),(7,4)]
            toRight (4,4) Black initBoard = [(5,4),(6,4),(7,4)]
            toRight (4,4) White initBoard = [(5,4),(6,4),(7,4)]
            -}
toRight :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: 8-x
toRight (x,y) clr brd 
    | x+1 == 8 = []
    | isEmpty $ getSquare (x+1,y) brd = (x+1,y): toRight (x+1,y) clr brd 
    | getColor (getSquare (x+1,y) brd) == clr = []
    | otherwise = [(x+1,y)]

{-diaBR (x,y) clr brd
    a function that finds all legal coordinates diagonally back right of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarBR (5,4) White initBoard = [(6,5)]
            diarBR (5,4) Black initBoard = [(6,5),(7,6)]
            diarBR (4,4) Black initBoard = [(5,5),(6,6)]
            diarBR (4,4) White initBoard = [(5,5)]
            -}
diagBR :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: 8-x && 8-y
diagBR (x,y) clr brd 
    | x+1 == 8 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x+1,y+1) brd = (x+1,y+1): diagBR (x+1,y+1) clr brd 
    | getColor (getSquare (x+1,y+1) brd) == clr = []
    | otherwise = [(x+1,y+1)]


{-diaFR(x,y) clr brd
    a function that finds all legal coordinates diagonally front right of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarFR (5,4) White initBoard = [(6,3),(7,2)]
            diarFR (5,4) Black initBoard = [(6,3),(7,2)]
            diarFR (4,4) Black initBoard = [(5,3),(6,2)]
            diarFR (4,4) White initBoard = [(5,3),(6,2),(7,1)]
            -}
diagFR :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: 8-x && y
diagFR (x,y) clr brd 
    | x+1 == 8 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x+1,y-1) brd = (x+1,y-1): diagFR (x+1,y-1) clr brd 
    | getColor (getSquare (x+1,y-1) brd) == clr = []
    | otherwise = [(x+1,y-1)]


{-diaBL(x,y) clr brd
    a function that finds all legal coordinates diagonally back left of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diarBL (5,4) White initBoard = [(4,5)]
            diarBL (5,4) Black initBoard = [(4,5),(3,6)]
            diarBL (4,4) Black initBoard = [(3,5),(2,6)]
            diarBL (4,4) White initBoard = [(3,5)]
            -}
diagBL :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: x && 8-y
diagBL (x,y) clr brd 
    | x-1 == -1 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x-1,y+1) brd = (x-1,y+1): diagBL (x-1,y+1) clr brd 
    | getColor (getSquare (x-1,y+1) brd) == clr = []
    | otherwise = [(x-1,y+1)]


{-diaFL(x,y) clr brd
    a function that finds all legal coordinates diagonally front left of (x,y) for clr on brd
  RETURNS: a list of tuples containing two ints
  EXAMPLES: diaFL (5,4) White initBoard = [(4,3),(3,2),(2,1)]
            diarFL (5,4) Black initBoard = [(4,3),(3,2)]
            diarFL (4,4) Black initBoard = [(3,3),(2,2)]
            diarFL (4,4) White initBoard = [(3,3),(2,2),(1,1)]
            -}
diagFL :: Coordinate -> PColor -> Board -> [Coordinate]
--VARIANT: x && y
diagFL (x,y) clr brd 
    | x-1 == -1 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x-1,y-1) brd = (x-1,y-1): diagFL (x-1,y-1) clr brd 
    | getColor (getSquare (x-1,y-1) brd) == clr = []
    | otherwise = [(x-1,y-1)]


{- rookMoves (x,y) clr brd
    a function that checks all possible moves for clr rook on (x,y) on brd and puts them in a list of coordinates
  RETURNS: A list of tuples containg two ints
  EXAMPLES: rookMoves (4,4) White initBoard = [(4,3),(4,2),(4,1),(4,5),(3,4),(2,4),(1,4),(0,4),(5,4),(6,4),(7,4)]
            rookMoves (4,4) Black initBoard = [(4,3),(4,2),(4,5),(4,6),(3,4),(2,4),(1,4),(0,4),(5,4),(6,4),(7,4)]
            rookMoves (3,3) White initBoard = [(3,2),(3,1),(3,4),(3,5),(2,3),(1,3),(0,3),(4,3),(5,3),(6,3),(7,3)]
            rookMoves (3,3) Black initBoard = [(3,2),(3,4),(3,5),(3,6),(2,3),(1,3),(0,3),(4,3),(5,3),(6,3),(7,3)]
-}
rookMoves :: Coordinate -> PColor -> Board -> [Coordinate]
rookMoves (x,y) clr brd = toFront (x,y) clr brd ++ toBack (x,y) clr brd ++ toLeft (x,y) clr brd ++ toRight (x,y) clr brd

{-
bishopMoves (x,y) clr brd
    a function that checks all possible moves for clr bishop on (x,y) on brd and puts them in a list of coordinates
  RETURNS : A list of tuples containing two ints
  EXAMPLES: bishopMoves (4,4) White initBoard = [(5,3),(6,2),(7,1),(3,3),(2,2),(1,1),(5,5),(3,5)]
            bishopMoves (4,4) Black initBoard = [(5,3),(6,2),(3,3),(2,2),(5,5),(6,6),(3,5),(2,6)]
            bishopMoves (3,3) White initBoard = [(4,2),(5,1),(2,2),(1,1),(4,4),(5,5),(2,4),(1,5)]
            bishopMoves (3,3) Black initBoard = [(4,2),(2,2),(4,4),(5,5),(6,6),(2,4),(1,5),(0,6)]
-}
bishopMoves :: Coordinate -> PColor -> Board -> [Coordinate]
bishopMoves (x,y) clr brd = diagFR (x,y) clr brd ++ diagFL (x,y) clr brd ++ diagBR (x,y) clr brd ++ diagBL (x,y) clr brd

{-queenMoves (x,y) clr brd 
    a function that checks all possible moves for clr queen on (x,y) on brd and puts them in a list of coordinates
  RETURNS : A list of coordinates
  EXAMPLES: queenMoves (4,4) White initBoard = [(4,3),(4,2),(4,1),(4,5),(3,4),(2,4),(1,4),(0,4),(5,4),(6,4),(7,4),(5,3),(6,2),(7,1),(3,3),(2,2),(1,1),(5,5),(3,5)]
            queenMoves (4,4) Black initBoard = [(4,3),(4,2),(4,5),(4,6),(3,4),(2,4),(1,4),(0,4),(5,4),(6,4),(7,4),(5,3),(6,2),(3,3),(2,2),(5,5),(6,6),(3,5),(2,6)]
            queenMoves (3,3) White initBoard = [(3,2),(3,1),(3,4),(3,5),(2,3),(1,3),(0,3),(4,3),(5,3),(6,3),(7,3),(4,2),(5,1),(2,2),(1,1),(4,4),(5,5),(2,4),(1,5)]
            queenMoves (3,3) Black initBoard = [(3,2),(3,4),(3,5),(3,6),(2,3),(1,3),(0,3),(4,3),(5,3),(6,3),(7,3),(4,2),(2,2),(4,4),(5,5),(6,6),(2,4),(1,5),(0,6)]
-}
queenMoves :: Coordinate -> PColor -> Board -> [Coordinate]
queenMoves (x,y) clr brd = rookMoves (x,y) clr brd ++ bishopMoves (x,y) clr brd


{-kingMoves (x,y) clr brd
    a function that checks all possible moves for clr king on (x,y) on brd and puts them in a list of coordinates
  RETURNS : A list of coordinates
  EXAMPLES: kingMoves (2,5) White initBoard = [(1,4),(3,4),(2,4),(3,5),(1,5)]
            kingMoves (2,5) Black initBoard = [(3,6),(1,4),(3,4),(1,6),(2,4),(2,6),(3,5),(1,5)]
            kingMoves (3,3) White initBoard = [(4,4),(2,2),(4,2),(2,4),(3,2),(3,4),(4,3),(2,3)]
            kingMoves (3,3) Black initBoard = [(4,4),(2,2),(4,2),(2,4),(3,2),(3,4),(4,3),(2,3)]-}
kingMoves :: Coordinate -> PColor -> Board -> [Coordinate]
kingMoves (x,y) clr brd = filter 
   (\x ->  isEmpty (getSquare x brd) || getColor (getSquare x brd) /= clr) (kingCoords (x,y))

{-kingCoords (x,y)
  Aux function for kingMoves which makes sure that the coordinates returned in kingMoves are valid, meaning that no int in a coordinte is < 0 or > 8
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS : A list of coordinates
  EXAMPLES: kingCoords (2,5)  = [(3,6),(1,4),(3,4),(1,6),(2,4),(2,6),(3,5),(1,5)]
            kingCoords (0,0)  = [(1,1),(0,1),(1,0)]
            kingCoords (3,3)  = [(4,4),(2,2),(4,2),(2,4),(3,2),(3,4),(4,3),(2,3)]
-}
kingCoords :: Coordinate -> [Coordinate]
kingCoords (x,y) = validSquares [(x+1,y+1),(x-1,y-1),(x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y),(x-1,y)]

{-castleMoves color board
    a function to get the coordinate for castling with color on board if is is availible
    RETURNS: if castling is availible it returns a list of the coordinates where the king can move to castle
             if it is not availible it returns an empty list
    EXAMPLES: castleMoves White initBoard == []
              casltemoves Black castleBoard == [(6,0),(2,0)]
-}
castleMoves :: PColor -> Board -> [Coordinate]
castleMoves clr brd = case (canCastleK clr brd, canCastleQ clr brd) of
                                    (True,True) -> if clr == White then [(6,7),(2,7)] else [(6,0),(2,0)]
                                    (True,_) -> if clr == White then [(6,7)] else [(6,0)]
                                    (_,True) -> if clr == White then [(2,7)] else [(2,0)]
                                    _ -> []

{-canCastleK color board
    a function to check if king side castling is availible for color on board
    RETURNS: True if there are no pieces between the rook and the king and they are both are unmoved
             False if the above is not true or if there is a piece attacking the square where the king has to pass
    EXAMPLES: canCastleK White initBoard == False
              canCastleK White castleBoard == True
-}
canCastleK :: PColor -> Board -> Bool
canCastleK White brd = not ((5,7) `elem` possibleMoves Black brd) && clearKSide White brd 
canCastleK Black brd = not ((5,0) `elem` possibleMoves White brd) && clearKSide Black brd 

{-canCastleQ color board
    a function to check if queen side castling is availible for color on board
    RETURNS: True if there are no pieces between the rook and the king and they are both are unmoved
             False if the above is not true or if there is a piece attacking the square where the king has to pass
    EXAMPLES: canCastleQ White initBoard == False
              canCastleQ White castleBoard == True
-}
canCastleQ :: PColor -> Board -> Bool
canCastleQ White brd = not ((3,7) `elem` possibleMoves Black brd) && clearQSide White brd
canCastleQ Black brd = not ((3,0) `elem` possibleMoves White brd) && clearQSide Black brd

{-knightMoves (x,y) clr brd
  a function that checks all possible moves for clr knight on (x,y) on brd and puts them in a list of coordinates
  RETURNS : A list of coordinates
  EXAMPLES: knightMoves (2,5) White initBoard = [(4,4),(3,3),(1,3)]
            knightMoves (2,5) Black initBoard = [(4,6),(4,4),(0,6),(0,6),(3,7),(3,3),(1,7),(1,3)]
            knightMoves (3,3) White initBoard = [(5,4),(5,2),(1,4),(1,4),(4,5),(4,1),(2,5),(2,1)]
            knightMoves (3,3) Black initBoard = [(5,4),(5,2),(1,4),(1,4),(4,5),(2,5)]
-}
knightMoves :: Coordinate -> PColor -> Board -> [Coordinate]
knightMoves (x,y) clr brd = filter 
   (\x ->  isEmpty (getSquare x brd) || getColor (getSquare x brd) /= clr) (knightCoords (x,y))

{-knightCoords (x,y)
  auxiliary function for knightMoves which makes sure that the coordinates returned in knightMoves are valid, meaning that no int in a coordinte is < 0 or > 8
  PRE: the coordinate must be between (0,0) and (7,7)
  RETURNS : A list of coordinates 
  EXAMPLES: knightCoords (2,5)  = [(4,6),(4,4),(0,6),(0,6),(3,7),(3,3),(1,7),(1,3)]
            knightCoords (0,0)  = [(2,1),(1,2)]
            knightCoords (3,3)  = [(5,4),(5,2),(1,4),(1,4),(4,5),(4,1),(2,5),(2,1)]
-}
knightCoords :: Coordinate -> [Coordinate]
knightCoords (x,y) = validSquares [(x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2, y-1),(x+1,y+2),(x+1,y-2),(x-1,y+2),(x-1,y-2)]

{-getKing clr brd
    a function that finds the coordinate for clr king on board
  RETURNS: a coordinate on the board 
  EXAMPLES: getKing White initBoard = (4,7)
            getKing Black initBoartd = (4,0)
            getKing Black testBoard = (3,0)
  -}
getKing :: PColor -> Board -> Coordinate
getKing clr brd = head (filter (\x -> getSquare x brd == Piece clr (King Moved) || getSquare x brd == Piece clr (King Unmoved)) [(x,y) | x <- [0..7], y <- [0..7]])

{-possibleMoves clr brd
    a function that takes a color and a board and returns a list of all possible coordinates all pieces of the same color can move to
    RETURNS: a list of tuples containing coordinates.
    EXAMPLES: possibleMoves White initBoard = [(0,5),(0,4),(1,5),(1,4),(2,5),(0,5),(2,5),(2,4),(3,5),(3,4),(4,5),(4,4),(5,5),(5,4),(6,5),(6,4),(7,5),(5,5),(7,5),(7,4)]
              possibleMoves Black initBoard = [(0,2),(0,3),(2,2),(0,2),(1,2),(1,3),(2,2),(2,3),(3,2),(3,3),(4,2),(4,3),(5,2),(5,3),(7,2),(5,2),(6,2),(6,3),(7,2),(7,3)]
              possibleMoves White castleBoard = [(0,5),(0,4),(1,7),(2,7),(3,7),(1,5),(1,4),(2,5),(2,4),(3,5),(3,4),(4,5),(4,4),(5,7),(3,7),(5,5),(5,4),(6,5),(6,4),(7,5),(7,4),(6,7),(5,7)]
    -}
possibleMoves :: PColor -> Board -> [Coordinate]
possibleMoves clr brd = concatMap (\x -> case getType (getSquare x brd) of 
        (Pawn _) -> pawnMoves x clr brd
        Knight -> knightMoves x clr brd
        Bishop -> bishopMoves x clr brd
        Queen -> queenMoves x clr brd
        (Rook _) -> rookMoves x clr brd
        (King _) -> kingMoves x clr brd)
        $ filter (\x -> getColor (getSquare x brd) == clr) [(x,y) | x <- [0..7], y <- [0..7]]

{-isChecked clr brd
    a function that checks if clr is checked on brd
    RETURNS: True or False
    EXAMPLES: isChecked White initBoard = False 
              isChecked Black testBoard = False
              isChecked White testBoard = True
-}
isChecked :: PColor -> Board -> Bool
isChecked clr brd = getKing clr brd `elem` possibleMoves (other clr) brd

{-getPromotedPawn clr brd
    a function that checks if clr pawn is on the other clr's back rank on brd and returns its coordinate.
    RETURNS: a list of coordinates
    EXAMPLES: getPromotedPawn White initBoard = []
              getPromtedPawn White testBoard2 = [(0,0)]
              getPromotedPawn Black testBoard2 = []

-}
getPromotedPawn :: PColor -> Board -> [Coordinate]
getPromotedPawn White brd = filter (\x -> getSquare x brd == Piece White (Pawn SingleMove)) [(x,y) | x <- [0..7], y <- [0]]
getPromotedPawn Black brd = filter (\x -> getSquare x brd == Piece Black (Pawn SingleMove)) [(x,y) | x <- [0..7], y <- [7]]


{-getKingSide clr brd
    a function that gets all squares for clr's king side on brd
  RETURNS: a list of squares containg a piece or empty.
  EXAMPLES: getKingSide White castleBoard = [♔, , ,♖]
            getKingSide Black initBoard = [♚,♝,♞,♜]

-}
getKingSide :: PColor ->  Board   -> [Square]
getKingSide White brd = map (\x -> getSquare x brd) [(x,y) | x <- [4..7], y <- [7]] 
getKingSide Black brd = map (\x -> getSquare x brd) [(x,y) | x <- [4..7], y <- [0]] 

{-clearKSide clr brd
    a function that checks if the king side for clr is clear on brd
    RETURNS: True or False
    Examples: clearKSide White initBoard = False
              clearKSide Black castleBoard = False
              clearKSide White castleBoard = True-}
clearKSide :: PColor -> Board -> Bool
clearKSide clr brd = (getKingSide clr brd) == [(Piece clr (King Unmoved)),(Empty),(Empty),(Piece clr (Rook Unmoved))]

{-getQueenSide clr brd
    a function that gets all squares for clr's queen side on brd
  RETURNS: a list of squares containg a piece or empty.
  EXAMPLES: getQueenSide White castleBoard = [[♖, , , ,♔]
            getQueenSide Black initBoard = [♜,♞,♝,♛,♚]

-}
getQueenSide :: PColor ->  Board   -> [Square]
getQueenSide White brd = map (\x -> getSquare x brd) [(x,y) | x <- [0..4], y <- [7]] 
getQueenSide Black brd = map (\x -> getSquare x brd) [(x,y) | x <- [0..4], y <- [0]] 

{-clearQSide clr brd
    a function that checks if the queen side for clr is clear on brd
    RETURNS: True or False
    EXAMPLES: clearQSide White initBoard = False
              clearQSide Black castleBoard = True
              clearQside White castleBoard = False
    -}
clearQSide :: PColor -> Board -> Bool
clearQSide clr brd = (getQueenSide clr brd) == [(Piece clr (Rook Unmoved)),(Empty),(Empty),(Empty),(Piece clr (King Unmoved))]


castleBoard :: Board
castleBoard = [[Piece Black (Rook Unmoved),Empty,Empty,Empty,Piece Black (King Unmoved),Empty,Empty,Piece Black (Rook Moved)],
             [Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove)],
             [Piece White (Rook Moved),Empty,Empty,Empty,Piece White (King Unmoved),Empty,Empty,Piece White (Rook Unmoved)]]



finishedWKcastleBoard :: Board
finishedWKcastleBoard = [[Piece Black (Rook Unmoved),Empty,Empty,Empty,Piece Black (King Unmoved),Empty,Empty,Piece Black (Rook Unmoved)],
             [Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove),Piece Black (Pawn SingleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove),Piece White (Pawn SingleMove)],
             [Piece White (Rook Unmoved),Empty,Empty,Empty,Empty,Piece White (Rook Moved),Piece White (King Moved),Empty]]
