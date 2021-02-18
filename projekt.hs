import Board
import Moves
import Debug.Trace

data Either a b = Left a | Right b 
data Exception = OutofBounds

type Exceptional a = Main.Either Exception a

throw :: Exception -> Exceptional a 
throw x = Main.Left x
{-stringToCoordinate str
   PRE: the char must from a to h and the inte muste be from 1 to 8 
a function that converts a char and a int into a tuple of ints in form of coordinates where the char is the first int in the tuple
and the int is the second int in the tuple
  RETURNS: a tuple of ints 
  EXAMPLES: stringToCoordinate "a4" = (0,4)
            stringToCoordinate "a1" = (0,7)
            stringToCoordinate "h1" = (7,7)

-}
stringToCoordinate :: String ->  Coordinate
stringToCoordinate "" = undefined
stringToCoordinate ('a':xs) =  (0,(8 - read xs))
stringToCoordinate ('b':xs) =  (1,(8 - read xs))
stringToCoordinate ('c':xs) =  (2,(8 - read xs))
stringToCoordinate ('d':xs) =  (3,(8 - read xs))
stringToCoordinate ('e':xs) =  (4,(8 - read xs))
stringToCoordinate ('f':xs) =  (5,(8 - read xs))
stringToCoordinate ('g':xs) =  (6,(8 - read xs))
stringToCoordinate ('h':xs) =  (7,(8 - read xs))
stringToCoordinate (_:xs) = undefined



{-coordinateToString 
A function that converts a Coordinate into a string of a char and a int
  PRE: the ints must be from 0 to 7 
  RETURNS: a string containing a char and a int
  EXAMPLES: coordinateToString (0,0) = "a8"
            coordinateToString (7,7) = "h1"
            coordinateToString (5,5) = "f3"

-}


coordinateToString :: Coordinate -> String 
coordinateToString (0,z) = 'a' : (show (8 - z))
coordinateToString (1,z) = 'b' : (show (8 - z))
coordinateToString (2,z) = 'c' : (show (8 - z))
coordinateToString (3,z) = 'd' : (show (8 - z))
coordinateToString (4,z) = 'e' : (show (8 - z))
coordinateToString (5,z) = 'f' : (show (8 - z))
coordinateToString (6,z) = 'g' : (show (8 - z))
coordinateToString (7,z) = 'h' : (show (8 - z))


{-initBoard
The chess boards startposition-}
initBoard :: Board
initBoard = [[Piece Black Rook,Piece Black Knight,Piece Black Bishop,Piece Black Queen,Piece Black King,Piece Black Bishop,Piece Black Knight,Piece Black Rook],
             [Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece White Pawn],
             [Piece White Rook,Piece White Knight,Piece White Bishop,Piece White Queen,Piece White King,Piece White Bishop,Piece White Knight,Piece White Rook]]

{-printBoard
a function that prints the current board to the terminal
-}
printBoard :: Board -> IO ()
printBoard board = putStrLn $ printBoard' 1 8 board

{-printBoard'
a function that makes a list of squares to a playable board-}
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

movePiece :: Board -> Coordinate -> Coordinate  -> IO Board
movePiece board crd1 crd2 = do
    let piece = getSquare crd1 board
        newboard = changeSquare crd1 board Empty
    return $ changeSquare crd2 newboard piece

play :: Board -> PColor -> IO ()
play brd clr = do
    printBoard brd
    putStrLn (show clr ++ " to play")
    (crd1,crd2) <- askMove
    newbrd <- playerTurn crd1 crd2 clr brd
    play newbrd $ other clr

            
playerTurn :: Coordinate  -> Coordinate  -> PColor -> Board -> IO Board 
playerTurn crd1 crd2 clr brd = do
    if not (isEmpty sqrcord1) && getColor sqrcord1 == clr
        then validMove clr (getType sqrcord1) crd1 crd2 brd
        else do 
                putStrLn $ "No " ++ show clr ++ " piece at coordinate"
                (crd1,crd2) <- askMove
                playerTurn crd1 crd2 clr brd
        where sqrcord1 = getSquare crd1 brd


askMove :: IO (Coordinate, Coordinate)
askMove = do
    putStrLn "Please input two valid coordinates"
    crd1 <- getLine
    crd2 <- getLine 
    if crd1 `elem` validInputs && crd2 `elem` validInputs
        then return (stringToCoordinate crd1,stringToCoordinate crd2)
        else do
            putStrLn "Either or both inputs are not a coordinate"
            askMove


validInputs = [x:show y | x <- ['a'..'h'], y <- [1..8]]

validMove :: PColor -> PType -> Coordinate -> Coordinate -> Board -> IO Board 
validMove clr piece crd1 crd2 brd = do
    let pieceMoves = case piece of
            Pawn -> pawnMoves crd1 clr brd
            Knight -> horseMoves crd1 clr brd
            Bishop -> bishopmoves crd1 clr brd
            Queen -> queenmoves crd1 clr brd
            Rook -> rookmoves crd1 clr brd
            King -> kingmoves crd1 clr brd
    if crd2 `elem` pieceMoves
        then movePiece brd crd1 crd2 
        else do 
            putStrLn "Invalid Move"
            (crd1,crd2) <- askMove
            playerTurn crd1 crd2 clr brd


