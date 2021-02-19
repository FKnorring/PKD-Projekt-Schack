import Board
import Moves
import Debug.Trace

data Either a b = Left a | Right b 
data Exception = OutofBounds

type Exceptional a = Main.Either Exception a

throw :: Exception -> Exceptional a 
throw x = Main.Left x
{-strToCoord str
   PRE: the char must from a to h and the inte muste be from 1 to 8 
a function that converts a char and a int into a tuple of ints in form of coordinates where the char is the first int in the tuple
and the int is the second int in the tuple
  RETURNS: a tuple of ints 
  EXAMPLES: strToCoord "a4" = (0,4)
            strToCoord "a1" = (0,7)
            strToCoord "h1" = (7,7)

-}
strToCoord :: String ->  Coordinate
strToCoord "" = undefined
strToCoord ('a':xs) =  (0,8 - read xs)
strToCoord ('b':xs) =  (1,8 - read xs)
strToCoord ('c':xs) =  (2,8 - read xs)
strToCoord ('d':xs) =  (3,8 - read xs)
strToCoord ('e':xs) =  (4,8 - read xs)
strToCoord ('f':xs) =  (5,8 - read xs)
strToCoord ('g':xs) =  (6,8 - read xs)
strToCoord ('h':xs) =  (7,8 - read xs)
strToCoord (_:xs) = undefined



{-coordToStr coordinate
A function that converts a Coordinate into a string of a char and a int
  PRE: the ints must be from 0 to 7 
  RETURNS: a string containing a char and a int
  EXAMPLES: coordToStr (0,0) = "a8"
            coordToStr (7,7) = "h1"
            coordToStr (5,5) = "f3"

-}


coordToStr :: Coordinate -> String 
coordToStr (0,z) = 'a' : show (8 - z)
coordToStr (1,z) = 'b' : show (8 - z)
coordToStr (2,z) = 'c' : show (8 - z)
coordToStr (3,z) = 'd' : show (8 - z)
coordToStr (4,z) = 'e' : show (8 - z)
coordToStr (5,z) = 'f' : show (8 - z)
coordToStr (6,z) = 'g' : show (8 - z)
coordToStr (7,z) = 'h' : show (8 - z)

strToPiece :: String -> PType 
strToPiece "q" = Queen
strToPiece "b" = Bishop
strToPiece "k" = Knight
strToPiece "r" = Rook

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
    mated <- isMated clr brd
    if mated
        then if isChecked clr brd 
            then do
            putStrLn (show clr ++ " is mated, the game is over")
            return ()
            else do
            putStrLn "Stalemate, the game is drawn"
            return ()
        else do   
            putStrLn (show clr ++ " to play")
            (crd1,crd2) <- askMove
            newbrd <- playerTurn crd1 crd2 clr brd
            newbrd' <- promote clr newbrd
            play newbrd' $ other clr

            
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
        then return (strToCoord crd1,strToCoord crd2)
        else do
            putStrLn "Either one or both inputs are not a valid coordinate"
            askMove


validInputs = [x:show y | x <- ['a'..'h'], y <- [1..8]]

validMove :: PColor -> PType -> Coordinate -> Coordinate -> Board -> IO Board 
validMove clr piece crd1 crd2 brd = do
    newbrd <- movePiece brd crd1 crd2
    let pieceMoves = case piece of
            Pawn -> pawnMoves crd1 clr brd
            Knight -> horseMoves crd1 clr brd
            Bishop -> bishopmoves crd1 clr brd
            Queen -> queenmoves crd1 clr brd
            Rook -> rookmoves crd1 clr brd
            King -> kingmoves crd1 clr brd
    if crd2 `elem` pieceMoves        
        then if isChecked clr newbrd
            then do
                putStrLn "Invalid Move, You are in check!"
                (crd1,crd2) <- askMove
                playerTurn crd1 crd2 clr brd
            else movePiece brd crd1 crd2
        else do 
            putStrLn "Invalid Move"
            (crd1,crd2) <- askMove
            playerTurn crd1 crd2 clr brd

isMated clr brd = do
            brds <- mapM (\x -> case getType (getSquare x brd) of 
                        Pawn -> mapM (movePiece brd x) (pawnMoves x clr brd)
                        Knight -> mapM (movePiece brd x) (horseMoves x clr brd)
                        Bishop -> mapM (movePiece brd x) (bishopmoves x clr brd)
                        Queen -> mapM (movePiece brd x) (queenmoves x clr brd)
                        Rook -> mapM (movePiece brd x) (rookmoves x clr brd)
                        King -> mapM (movePiece brd x) (kingmoves x clr brd))
                        $ filter (\x -> getColor (getSquare x brd) == clr) [(x,y) | x <- [0..7], y <- [0..7]]
            let allbrds = concat brds
            return $ not (False `elem` map (isChecked clr) allbrds)

promote :: PColor -> Board -> IO Board 
promote clr brd = do
            if null (getPromotedPawn clr brd)
                then return brd
                else do 
                    piece <- askPromote
                    return $ changeSquare (head (getPromotedPawn clr brd)) brd (case piece of
                                                        Queen -> Piece clr Queen 
                                                        Rook -> Piece clr Rook 
                                                        Bishop -> Piece clr Bishop 
                                                        Knight -> Piece clr Knight)

askPromote :: IO PType
askPromote = do
    putStrLn "What do you want to promote to?"
    putStrLn "Write q for queen, b for bishop, k for knight or r for rook"
    promote <- getLine
    if promote `elem` ["q","b","k","r"]
        then return $ strToPiece promote
        else askPromote




kCastle :: PColor -> Board -> IO Board
kCastle White brd = do 
    newbrd <-movePiece brd (4,7) (6,7)
    movePiece newbrd (7,7) (5,7)
kCastle Black brd = do 
    newbrd <- movePiece brd (4,0) (6,0)
    movePiece newbrd (7,0) (5,0)

qCastle :: PColor -> Board -> IO Board
qCastle White brd = do 
    newbrd <- movePiece brd (4,7) (2,7)
    movePiece newbrd (0,7) (3,7)
qCastle Black brd = do 
    newbrd <- movePiece brd (4,0) (2,0)
    movePiece newbrd (0,0) (3,0)


canCastleK :: PColor -> Board -> Bool
canCastleK White brd = not ((5,7) `elem` possibleMoves Black brd) && clearKSide White brd 
canCastleK Black brd = not ((5,0) `elem` possibleMoves White brd) && clearKSide Black brd 

canCastleQ :: PColor -> Board -> Bool
canCastleQ White brd = not ((3,7) `elem` possibleMoves Black brd) && clearQSide White brd
canCastleQ Black brd = not ((3,0) `elem` possibleMoves White brd) && clearQSide Black brd

castleBoard' = [[Piece Black Rook,Empty,Empty,Empty,Piece Black King,Empty,Empty,Piece Black Rook],
             [Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn,Piece Black Pawn],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White Pawn,Piece White Pawn,Piece White Pawn,Piece Black Rook,Piece White Pawn,Piece Black Rook,Piece White Pawn,Piece White Pawn],
             [Piece White Rook,Empty,Empty,Empty,Piece White King,Empty,Empty,Piece White Rook]]

kingSideCastle :: PColor -> Board -> IO Board
kingSideCastle clr brd = do
    if isChecked clr brd 
        then do
            putStrLn "You cant castle, you are in check "
            (cord1,cord2) <- askMove
            playerTurn cord1 cord2 clr brd 
        else 
            if canCastleK clr brd 
                then do
                    newbrd <- kCastle clr brd 
                    if isChecked clr newbrd 
                        then do
                            putStrLn "You cant castle, you will be in check "
                            (cord1,cord2) <- askMove
                            playerTurn cord1 cord2 clr brd
                        else return newbrd 
                else do
                        putStrLn "You cant castle, castling is blocked "
                        (cord1,cord2) <- askMove
                        playerTurn cord1 cord2 clr brd

queenSideCastle :: PColor -> Board -> IO Board
queenSideCastle clr brd = do
    if isChecked clr brd 
        then do
            putStrLn "You cant castle, you are in check "
            (cord1,cord2) <- askMove
            playerTurn cord1 cord2 clr brd 
        else 
            if canCastleQ clr brd 
                then do
                    newbrd <- qCastle clr brd 
                    if isChecked clr newbrd 
                        then do
                            putStrLn "You cant castle, you will be in check "
                            (cord1,cord2) <- askMove
                            playerTurn cord1 cord2 clr brd
                        else return newbrd
                else do
                        putStrLn "You cant castle, castling is blocked "
                        (cord1,cord2) <- askMove
                        playerTurn cord1 cord2 clr brd