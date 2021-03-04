module Chess where
import Board
import Moves

import Test.HUnit
{-main
A function to start the game-}
main :: IO () 
main = play initBoard White 

{-strToCoord str
   PRE: the char must from a to h and the int must be from 1 to 8 
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

{-strToPiece string
  makes a PType from a string 
  PRE: the string must be "q", "b", "k" or "r"
  RETURNS: Certain PTypes used for promotion
  EXAMPLES: strToPiece "q" = Queen
            strToPiece "b" = Bishop
            strToPiece "k" = Knight
            strToPiece "r" = Rook Moved

-}
strToPiece :: String -> PType 
strToPiece "q" = Queen
strToPiece "b" = Bishop
strToPiece "k" = Knight
strToPiece "r" = Rook Moved

{-initBoard
The chess boards startposition-}
initBoard :: Board
initBoard = [[Piece Black (Rook Unmoved ),Piece Black Knight,Piece Black Bishop,Piece Black Queen,Piece Black (King Unmoved),Piece Black Bishop,Piece Black Knight,Piece Black (Rook Unmoved )],
             [Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove )],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove )],
             [Piece White (Rook Unmoved ),Piece White Knight,Piece White Bishop,Piece White Queen,Piece White (King Unmoved),Piece White Bishop,Piece White Knight,Piece White (Rook Unmoved )]]

{-printBoard color board
    a function that prints a board to the terminal for colors perspective
    PRE: type in "chcp.com 65001" in terminal before ghci so u it can print the pieces. If you are on Windows
    RETURNS: an IO action that prints multiple lines of strings 
-}
printBoard :: PColor -> Board -> IO ()
printBoard clr brd = putStrLn (case clr of
        White -> printWhiteBoard' 1 8 brd
        Black -> printBlackBoard' 1 8 brd)

{-printWhiteBoard' x y board
    a function that turns a board into a string
    PRE: x = 1, y = 8 to make a correct board
    RETURNS: a string of the board
-}
printWhiteBoard' :: Int -> Int -> Board -> String 
--VARIANT: Length of Board
printWhiteBoard' 1 8 ((a:xs):xss) = "  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n8 ║"++show a++" "   ++ printWhiteBoard' 2 8 (xs:xss)
printWhiteBoard' 1 y ((a:xs):xss) = show y++" ║"++show a++" "                         ++ printWhiteBoard' 2 y (xs:xss)
printWhiteBoard' x y ((a:xs):xss) =  "║" ++ show a ++" "                              ++ printWhiteBoard' (x+1) y (xs:xss)
printWhiteBoard' _ 1 ([]:xs)      =  "║\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n"               ++ printWhiteBoard' 1 1 xs
printWhiteBoard' x y ([]:xs)      =  "║\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n"               ++ printWhiteBoard' 1 (y-1) xs
printWhiteBoard' x y []           =  "   a  b  c  d  e  f  g  h"

{-printBlackBoard' x y board
    a function that turns a board into a string
    PRE: x = 1, y = 8 to make a correct board
    RETURNS: a string of the board
-}
printBlackBoard' :: Int -> Int -> Board -> String 
--VARIANT: Length of Board
printBlackBoard' 1 8 ((a:xs):xss) = printBlackBoard' 2 8 (xs:xss) ++ ("║" ++ show a++" ║" ++  "\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n    h  g  f  e  d  c  b  a\n") 
printBlackBoard' 1 y ((a:xs):xss) = printBlackBoard' 2 y (xs:xss) ++ ("║"++show a++" ║ ")                       
printBlackBoard' x y ((a:xs):xss) = printBlackBoard' (x+1) y (xs:xss) ++ ("║"++show a++" ")                                
printBlackBoard' _ 1 ([]:xs)      = printBlackBoard' 1 1 xs ++ "  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n1 "               
printBlackBoard' x y ([]:xs)      = printBlackBoard' 1 (y-1) xs ++ "\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n" ++ show y ++ " "           
printBlackBoard' x y []           =  ""


{-changeSquare coordinate board square
 a function that takes a coordinate from a board and changes whats on that coordinate to square
 RETURNS: A new board with the differnece being the square that is changed within the argument board
 EXAMPLES: head $ changeSquare (0,0) initBoard (Piece Black Knight) = [♞,♞,♝,♛,♚,♝,♞,♜]
           head $ changeSquare (0,0) initBoard (Piece Black Queen) = [♛,♞,♝,♛,♚,♝,♞,♜]
 -}
changeSquare :: Coordinate -> Board -> Square -> Board
--VARIANT: y
changeSquare (x,0) (a:xs) square = changeSquare' x a square:xs
changeSquare (x,y) (a:xs) square = a : changeSquare (x,y-1) xs square

{-changeSquare' x row square 
 a function that replaces index x on row to square
 PRE: must be a valid int, from 0 to 7.
 RETURNS: A list of squares
 EXAMPLES: changeSquare' 3 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] (Piece White Queen)
          = [ , , ,♕, , , , ]
           changeSquare' 0 [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] (Piece White Knight)
           = [♘, , , , , , , ]
-}
changeSquare' :: Int -> [Square] -> Square -> [Square]
--VARIANT: x
changeSquare' 0 (a:xs) square = square:xs 
changeSquare' x (a:xs) square = a:changeSquare' (x-1) xs square

{-enPassant clr brd crd1 crd2 
    a function that checks if the enPassant move is available
    RETURNS: True if en passant is availibe
             False if it is not availible
    EXAMPLES:  enPassant White testBoard (6,3) (7,2) = True
               enPassant Black testBoard (6,3) (7,2) = False-}
enPassant :: PColor -> Board -> Coordinate -> Coordinate -> Bool
enPassant clr brd crd1 crd2 = (isEmpty (getSquare crd2 brd) && (front || back)) && isPawn
    where front = case snd crd2 of
              0 -> False
              _ -> getSquare (fst crd2,snd crd2 - 1) brd == Piece (other clr) (Pawn DoubleMove)
          back = case snd crd2 of
              7 -> False
              _ -> getSquare (fst crd2,snd crd2 + 1) brd == Piece (other clr) (Pawn DoubleMove)
          isPawn = getSquare crd1 brd == Piece clr (Pawn SingleMove) && abs (snd crd1 - snd crd2) < 2 
          


{-movePiece board crd1 crd2
    a function to move a piece from crd1 to crd2 on board
    RETURNS: a new IO Board from board
-}
movePiece :: Board -> Coordinate -> Coordinate  -> IO Board
movePiece board crd1 crd2 = do
    let piece = getSquare crd1 board
        clr = getColor piece 
        newboard = changeSquare crd1 board Empty
    case (crd1,crd2,canCastleK clr board,canCastleQ clr board) of
        ((4,0),(6,0),True,_) -> return $ kCastle clr board
        ((4,0),(2,0),_,True) -> return $ qCastle clr board
        ((4,7),(6,7),True,_) -> return $ kCastle clr board
        ((4,7),(2,7),_,True) -> return $ qCastle clr board
        _                    -> 
            if enPassant clr board crd1 crd2
                then do
                    return $ changeSquare crd2 (removeDoublePawn clr newboard) piece
                else return $ changeSquare crd2 newboard (case piece of
                                Piece clr (Rook Unmoved) -> Piece clr (Rook Moved)
                                Piece clr (King Unmoved) -> Piece clr (King Moved)
                                Piece clr (Pawn SingleMove) -> if abs (snd crd1 - snd crd2) == 2
                                                                    then Piece clr (Pawn DoubleMove)
                                                                    else Piece clr (Pawn SingleMove)
                                Piece clr (Pawn DoubleMove) -> Piece clr (Pawn SingleMove)
                                _ -> piece)
    where   kCastle White brd = do 
                changeSquare crd1 (changeSquare (5,7) (changeSquare (7,7) (changeSquare crd2 brd (Piece White (King Moved))) Empty) (Piece White (Rook Moved))) Empty
            kCastle Black brd = do 
                changeSquare crd1 (changeSquare (5,0) (changeSquare (7,0) (changeSquare crd2 brd (Piece Black (King Moved))) Empty) (Piece Black (Rook Moved))) Empty
            qCastle White brd = do 
                changeSquare crd1 (changeSquare (3,7) (changeSquare (0,7) (changeSquare crd2 brd (Piece White (King Moved))) Empty) (Piece White (Rook Moved))) Empty
            qCastle Black brd = do 
                changeSquare crd1 (changeSquare (3,0) (changeSquare (0,0) (changeSquare crd2 brd (Piece Black (King Moved))) Empty) (Piece Black (Rook Moved))) Empty
    
{-removeDoublePawn clr brd
a function that changes the square containing a doublePawn of the opposite color to a empty square
   RETURNS: updated board
   EXAMPLES:  removeDoublePawn White testBoard = [ , , , , , ,♙, ] Note: only list with changes
              removeDoublePawn Black testBoard = [ , , , , , ,♙,♟] Note: only list with changes
 -}
removeDoublePawn :: PColor -> Board -> Board 
removeDoublePawn clr brd = if null doublepawn then brd else changeSquare (head doublepawn) brd Empty
    where doublepawn = filter (\x -> getSquare x brd == Piece (other clr) (Pawn DoubleMove)) [(x,y) | x <- [0..7], y <- [0..7]] 

{-resetDoubleMove clr brd
a function that changes a pawn doubleMove to a pawn singleMove
 RETURNS: Updated board 
-}
resetDoubleMove :: PColor -> Board -> Board 
resetDoubleMove clr brd = if null doublepawn then brd else changeSquare (head doublepawn) brd (Piece clr (Pawn SingleMove))
    where doublepawn = filter (\x -> getSquare x brd == Piece clr (Pawn DoubleMove)) [(x,y) | x <- [0..7], y <- [0..7]] 

{-play brd clr
    play function allows a clr to start the game and make the first move on a specific brd
   EXAMPLES: play initBoard White :  Starts a standard chess game -}
play :: Board -> PColor -> IO ()
play brd clr = do
    let brd' = resetDoubleMove clr brd
    printBoard clr brd
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
            newbrd <- playerTurn crd1 crd2 clr brd'
            newbrd' <- promote clr newbrd
            play newbrd' $ other clr

{-playerTurn crd1 crd2 clr brd
    a function that performs one turn for a color on board by checking if a move from crd1 to crd2 is legal.
-}       
playerTurn :: Coordinate  -> Coordinate  -> PColor -> Board -> IO Board 
playerTurn crd1 crd2 clr brd = do
    if not (isEmpty sqrcord1) && getColor sqrcord1 == clr
        then validMove clr (getType sqrcord1) crd1 crd2 brd
        else do 
                putStrLn $ "No " ++ show clr ++ " piece at coordinate"
                makeMove clr brd
        where sqrcord1 = getSquare crd1 brd

{-askMove
    a function that ask a player to make a specific move and returns a coordinate if both inputs are a valid coordinate on the board. 
-}
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

{-makeMove clr brd
    a function that a promts clr for two coordinates and then sends it to playerTurn-}
makeMove :: PColor -> Board -> IO Board
makeMove clr brd = do
        (crd1,crd2) <- askMove
        playerTurn crd1 crd2 clr brd
    

validInputs = [x:show y | x <- ['a'..'h'], y <- [1..8]]

{-validMove color piece firstcoordinate secondcoordinate board
    a function to make a move for color with piece from first coordinate to second coordinate on board if it is valid
    RETURNS: a new board if a valid move is made
             outputs "Invalid Move" if the move is invalid and then asks for new coordinates
-}
validMove :: PColor -> PType -> Coordinate -> Coordinate -> Board -> IO Board 
validMove clr piece crd1 crd2 brd = do
        newbrd <- movePiece brd crd1 crd2
        let pieceMoves = case piece of
                (Pawn _) -> pawnMoves crd1 clr brd
                Knight -> knightmoves crd1 clr brd
                Bishop -> bishopmoves crd1 clr brd
                Queen -> queenmoves crd1 clr brd
                (Rook _) -> rookmoves crd1 clr brd
                (King _) -> kingmoves crd1 clr brd ++ castlemoves clr brd
        if crd2 `elem` pieceMoves        
            then if isChecked clr newbrd
                then do
                    putStrLn "Invalid Move"
                    makeMove clr brd
                else return newbrd
            else do 
                putStrLn "Invalid Move"
                makeMove clr brd

{-isMated clr brd
    a function to check if clr is mated on brd
-}
isMated :: PColor -> Board -> IO Bool
isMated clr brd = do
            brds <- mapM (\x -> case getType (getSquare x brd) of 
                        (Pawn _) -> mapM (movePiece brd x) (pawnMoves x clr brd)
                        Knight -> mapM (movePiece brd x) (knightmoves x clr brd)
                        Bishop -> mapM (movePiece brd x) (bishopmoves x clr brd)
                        Queen -> mapM (movePiece brd x) (queenmoves x clr brd)
                        (Rook _) -> mapM (movePiece brd x) (rookmoves x clr brd)
                        (King _) -> mapM (movePiece brd x) (kingmoves x clr brd))
                        $ filter (\x -> getColor (getSquare x brd) == clr) [(x,y) | x <- [0..7], y <- [0..7]]
            let allbrds = concat brds
            return $ notElem False (map (isChecked clr) allbrds)
{-promote clr brd
    a function that changes a pawn to another piece if it has reached the opposite back rank
-}
promote :: PColor -> Board -> IO Board 
promote clr brd = do
            if null (getPromotedPawn clr brd)
                then return brd
                else do
                    piece <- askPromote
                    return $ changeSquare (head (getPromotedPawn clr brd)) brd (Piece clr piece)
                                                        
{-askPromote
    a function to prompt the player for a piece and then returns the players choice
-}
askPromote :: IO PType
askPromote = do
    putStrLn "What do you want to promote to?"
    putStrLn "Write q for queen, b for bishop, k for knight or r for rook"
    promote <- getLine
    if promote `elem` ["q","b","k","r"]
        then return $ strToPiece promote
        else askPromote


testBoard :: Board
testBoard = [[Empty,Piece Black Knight,Piece Black Bishop,Piece Black (King Unmoved),Piece Black Queen ,Piece Black Bishop,Piece Black Knight,Piece Black (Rook Unmoved )],
             [Piece White (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty,Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Piece White (Pawn SingleMove),Piece Black (Pawn DoubleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Empty,Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove )],
             [Piece White (Rook Unmoved ),Piece White Knight,Piece White Bishop,Piece White Queen,Piece White (King Unmoved),Piece White Bishop,Piece White Knight,Piece White (Rook Unmoved )]]



testBoard2 :: Board
testBoard2 = [[Piece White (Pawn SingleMove),Piece Black Knight,Piece Black Bishop,Piece Black (King Unmoved),Piece Black Queen ,Piece Black Bishop,Piece Black Knight,Piece Black (Rook Unmoved )],
             [Piece White (Pawn SingleMove),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty,Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Piece White (Pawn SingleMove),Piece Black (Pawn DoubleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Empty,Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove )],
             [Piece White (Rook Unmoved ),Piece White Knight,Piece White Bishop,Piece White Queen,Piece White (King Unmoved),Piece White Bishop,Piece White Knight,Piece White (Rook Unmoved )]]


runtests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18]

test1 = TestCase $ assertEqual "Looks if the safe Black king is in check on startboard" False (isChecked White initBoard)

test2 = TestCase $ assertEqual "Looks if the white king is in check on testboard" True (isChecked White testBoard)

test3 = TestCase $ assertEqual "Looks if the safe Black king is safe on testBoard" False (isChecked Black testBoard)

test4 = TestCase $ assertEqual "looks if squares is empty between king and rook, kingside for white  " False (canCastleK White initBoard)

test5 = TestCase $ assertEqual "looks if squares is empty between king and rook, kingside for Black " False (canCastleK Black castleBoard )

test6 = TestCase $ assertEqual "looks if squares is empty between king and rook, queenside for White " False (canCastleQ White castleBoard)

test7 = TestCase $ assertEqual "all possible moves for white at the start" [(0,5),(0,4),(1,5),(1,4),(2,5),(0,5),(2,5),(2,4),(3,5),(3,4),(4,5),(4,4),(5,5),(5,4),(6,5),(6,4),(7,5),(5,5),(7,5),(7,4)](possibleMoves White initBoard)

test8 = TestCase $ assertEqual "Checks if pawnMoves function finds all possible moves for the white pawn at coordinate (0,6)" [(0,5),(0,4)] (pawnMoves (0,6) White initBoard)

test9 = TestCase $ assertEqual "checks if promotedPawn function finds the correct promotedPawn on the testBoard" [] (getPromotedPawn White testBoard)

test10 = TestCase $ assertEqual "checks if promotedPawn function finds the correct promotedPawn on the testBoard" [] (getPromotedPawn Black testBoard)

test11 = TestCase $ assertEqual "Checks the White knights possible moves on the startboard" [(2,5),(0,5)] (knightmoves (1,7) White initBoard)

test12 = TestCase $ assertEqual "finds the square the pawn can move to make the move enPassant" [(7,2)] (enPassantSquare (6,3) White testBoard)

test13 = TestCase $ assertEqual "Checks the White kings possible moves on the testBoard" [(4,6)] (kingmoves (4,7) White testBoard)

test14 = TestCase $ assertEqual "test the black kings possible moves on testBoard " [(4,1)] (kingmoves (3,0) Black testBoard)

test15 = TestCase $ assertEqual "test strTocord function for a number of stings" [(0,8),(1,1),(2,3),(1,5)] (map strToCoord ["a0", "b7","c5","b3"])

test16 = TestCase $ assertEqual "Test if enPassant returns coorect bool for specific coordinate (6,3) (7,2) returns True for testBoard" True (enPassant White testBoard (6,3) (7,2))

test17 =  TestCase $ assertEqual "Test if resetDoubleMove removes all double pawns for a color from a board" [] (concatMap (filter (== (Piece Black (Pawn DoubleMove)))) (resetDoubleMove Black testBoard))

test18 = TestCase $ assertEqual "Test if changeSquare works" (Piece White Bishop) (getSquare (0,0) $ changeSquare (0,0) initBoard (Piece White Bishop))
