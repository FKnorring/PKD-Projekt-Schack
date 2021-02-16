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

 
