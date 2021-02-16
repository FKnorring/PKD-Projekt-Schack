import Board
import Moves

stringToCoordinate :: String -> Coordinate
stringToCoordinate "" = undefined
stringToCoordinate ('a':xs) = (0,(8 - read xs))
stringToCoordinate ('b':xs) = (1,(8 - read xs))
stringToCoordinate ('c':xs) = (2,(8 - read xs))
stringToCoordinate ('d':xs) = (3,(8 - read xs))
stringToCoordinate ('e':xs) = (4,(8 - read xs))
stringToCoordinate ('f':xs) = (5,(8 - read xs))
stringToCoordinate ('g':xs) = (6,(8 - read xs))
stringToCoordinate ('h':xs) = (7,(8 - read xs))




coordinateToString :: Coordinate -> String 
coordinateToString (0,z) = 'a' : (show (8 - z))
coordinateToString (1,z) = 'b' : (show (8 - z))
coordinateToString (2,z) = 'c' : (show (8 - z))
coordinateToString (3,z) = 'd' : (show (8 - z))
coordinateToString (4,z) = 'e' : (show (8 - z))
coordinateToString (5,z) = 'f' : (show (8 - z))
coordinateToString (6,z) = 'g' : (show (8 - z))
coordinateToString (7,z) = 'h' : (show (8 - z))

initBoard :: Board
initBoard = [[Occupied (Piece Black Rook),Occupied (Piece Black Knight),Occupied (Piece Black Bishop),Occupied (Piece Black Queen),Occupied (Piece Black King),Occupied (Piece Black Bishop),Occupied (Piece Black Knight),Occupied (Piece Black Rook)],
             [Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn),Occupied (Piece Black Pawn)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn),Occupied (Piece White Pawn)],
             [Occupied (Piece White Rook),Occupied (Piece White Knight),Occupied (Piece White Bishop),Occupied (Piece White Queen),Occupied (Piece White King),Occupied (Piece White Bishop),Occupied (Piece White Knight),Occupied (Piece White Rook)]]

printBoard :: Board -> IO ()
printBoard board = putStrLn $ printBoard' 1 8 board

printBoard' :: Int -> Int -> Board -> String 
printBoard' 1 8 ((a:xs):xss) = ("  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n8 ║"++show a++" ") ++ printBoard' 2 8 (xs:xss)
printBoard' 1 y ((a:xs):xss) = (show y++" ║"++show a++" ")                       ++ printBoard' 2 y (xs:xss)
printBoard' x y ((a:xs):xss) = ("║"++show a++" ")                                ++ printBoard' (x+1) y (xs:xss)
printBoard' _ 1 ([]:xs)      =  "║\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n"               ++ printBoard' 1 1 xs
printBoard' x y ([]:xs)      =  "║\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n"               ++ printBoard' 1 (y-1) xs
printBoard' x y []           =  "   a  b  c  d  e  f  g  h"

changeSquare :: Coordinate -> Board -> Square -> Board
changeSquare (x,0) (a:xs) square = changeSquare' x a square:xs
changeSquare (x,y) (a:xs) square = a : changeSquare (x,y-1) xs square

changeSquare' :: Int -> [Square] -> Square -> [Square]
changeSquare' 0 (a:xs) square = square:xs 
changeSquare' x (a:xs) square = a:changeSquare' (x-1) xs square

movePiece :: Board -> IO Board
movePiece board = do
    cord <- getLine
    let realCord = stringToCoordinate cord
        piece = getSquare realCord board
        newboard = changeSquare realCord board Empty
    newcord <- getLine
    let realNewCord = stringToCoordinate newcord
    return $ changeSquare realNewCord newboard piece

rookmoves :: Coordinate -> PColor -> Board -> [Coordinate]
rookmoves (x,y) clr brd = toFront (x,y) clr brd ++ toBack (x,y) clr brd ++ toLeft (x,y) clr brd ++ toRight (x,y) clr brd

toFront :: Coordinate -> PColor -> Board -> [Coordinate]
toFront (x,y) clr brd 
    | y-1 == -1 = []
    | isEmpty $ getSquare (x,y-1) brd = (x,y-1): toFront (x,y-1) clr brd 
    | getColor (getSquare (x,y-1) brd) == clr = []
    | otherwise = [(x,y-1)]

toBack :: Coordinate -> PColor -> Board -> [Coordinate]
toBack (x,y) clr brd 
    | y+1 == 8 = []
    | isEmpty $ getSquare (x,y+1) brd = (x,y+1): toBack (x,y+1) clr brd 
    | getColor (getSquare (x,y+1) brd) == clr = []
    | otherwise = [(x,y-1)]

toLeft :: Coordinate -> PColor -> Board -> [Coordinate]
toLeft (x,y) clr brd 
    | x-1 == -1 = []
    | isEmpty $ getSquare (x-1,y) brd = (x-1,y): toLeft (x-1,y) clr brd 
    | getColor (getSquare (x-1,y) brd) == clr = []
    | otherwise = [(x-1,y)]

toRight :: Coordinate -> PColor -> Board -> [Coordinate]
toRight (x,y) clr brd 
    | x+1 == 8 = []
    | isEmpty $ getSquare (x+1,y) brd = (x+1,y): toRight (x+1,y) clr brd 
    | getColor (getSquare (x+1,y) brd) == clr = []
    | otherwise = [(x+1,y)]

diagBR :: Coordinate -> PColor -> Board -> [Coordinate]
diagBR (x,y) clr brd 
    | x+1 == 8 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x+1,y+1) brd = (x+1,y+1): diagBR (x+1,y+1) clr brd 
    | getColor (getSquare (x+1,y+1) brd) == clr = []
    | otherwise = [(x+1,y+1)]

diagFR :: Coordinate -> PColor -> Board -> [Coordinate]
diagFR (x,y) clr brd 
    | x+1 == 8 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x+1,y-1) brd = (x+1,y-1): diagFR (x+1,y-1) clr brd 
    | getColor (getSquare (x+1,y-1) brd) == clr = []
    | otherwise = [(x+1,y-1)]

diagBL :: Coordinate -> PColor -> Board -> [Coordinate]
diagBL (x,y) clr brd 
    | x-1 == -1 = []
    | y+1 == 8 = []
    | isEmpty $ getSquare (x-1,y+1) brd = (x-1,y+1): diagBL (x-1,y+1) clr brd 
    | getColor (getSquare (x-1,y+1) brd) == clr = []
    | otherwise = [(x-1,y+1)]

diagFL :: Coordinate -> PColor -> Board -> [Coordinate]
diagFL (x,y) clr brd 
    | x-1 == -1 = []
    | y-1 == -1 = []
    | isEmpty $ getSquare (x-1,y-1) brd = (x-1,y-1): diagFL (x-1,y-1) clr brd 
    | getColor (getSquare (x-1,y-1) brd) == clr = []
    | otherwise = [(x-1,y-1)]


bishopmoves :: Coordinate -> PColor -> Board -> [Coordinate]
bishopmoves (x,y) clr brd = diagFR (x,y) clr brd ++ diagFL (x,y) clr brd ++ diagBR (x,y) clr brd ++ diagBL (x,y) clr brd

queenmoves :: Coordinate -> PColor -> Board -> [Coordinate]
queenmoves (x,y) clr brd = rookmoves (x,y) clr brd ++ bishopmoves (x,y) clr brd

kingmoves :: Coordinate -> PColor -> Board -> [Coordinate]
kingmoves (x,y) White brd = filter 
   (\x ->  (isEmpty (getSquare x brd)) || getColor (getSquare x brd) /= White) (kingmoves' (x,y))
kingmoves (x,y) Black brd = filter 
   (\x ->  (isEmpty (getSquare x brd)) || getColor (getSquare x brd) /= Black) (kingmoves' (x,y))
  


kingmoves' :: Coordinate -> [Coordinate]
kingmoves' (x,y) = filter (`elem` (allSquarelist)) [(x+1,y+1),(x-1,y-1),(x+1,y-1),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y),(x-1,y)]

allSquarelist :: [Coordinate]
allSquarelist  = [(x,y) | x <- [0..7], y <- [0..7]]
