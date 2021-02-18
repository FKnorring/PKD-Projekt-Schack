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




    


movePiece :: Board -> Coordinate -> Coordinate  -> IO Board
movePiece board crd1 crd2 = do
    let piece = getSquare crd1 board
        newboard = changeSquare crd1 board Empty
    return $ changeSquare crd2 newboard piece

 
play brd = do
    printBoard brd
    putStrLn "White to play"
    crd1 <- getLine
    crd2 <- getLine 
    if null crd1 || null crd2 
        then return ()
        else do
            newbrd <- whitesMove crd1 crd2 brd
            play newbrd
    
            
whitesMove :: String -> String -> Board -> IO Board 
whitesMove crd1 crd2 brd = do
    let cord1 = stringToCoordinate crd1
        cord2 = stringToCoordinate crd2
    if not (isEmpty (getSquare cord1 brd)) && getColor (getSquare cord1 brd) == White
        then case getSquare cord1 brd of
            Occupied (Piece White Pawn) -> if cord2 `elem` pawnMoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
            Occupied (Piece White Rook) -> if cord2 `elem` rookmoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
            Occupied (Piece White Bishop) -> if cord2 `elem` bishopmoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
            Occupied (Piece White Queen) -> if cord2 `elem` queenmoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
            Occupied (Piece White Knight) -> if cord2 `elem` horseMoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
            Occupied (Piece White King) -> if cord2 `elem` kingmoves cord1 White brd
                then movePiece brd cord1 cord2 
                else do 
                        putStrLn "Invalid Move"
                        repeat
        else do 
                putStrLn "No white piece at coordinate"
                putStrLn "Input move again:"
                repeat
        where repeat = do crd1 <- getLine
                          crd2 <- getLine 
                          if null crd1 || null crd2 
                            then do
                                putStrLn "You need to input two coordinates"
                                repeat
                            else do
                                whitesMove crd1 crd2 brd

