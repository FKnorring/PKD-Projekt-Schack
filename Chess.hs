module Chess where
import Board
import Moves
import Debug.Trace


--import Test.HUnit

main :: IO () 
main = play initBoard White 
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

{-strToPiece string
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
    a function that prints a board to the terminal, prints from whites perspecitve if the first Argument is White, if its Black it prints
    from blacks perspective
    PRE: type in "chcp.com 65001" in terminal before ghci so u it can print the pieces. 
    RETURNS: Multiple strings in the terminal
    EXAMPLES: 
-}
printBoard :: PColor -> Board -> IO ()
printBoard clr brd = putStrLn (case clr of
        White -> printWhiteBoard' 1 8 brd
        Black -> printBlackBoard' 1 8 brd)

{-printWhiteBoard'
a function that makes a list of squares to a playable board-}
printWhiteBoard' :: Int -> Int -> Board -> String 
printWhiteBoard' 1 8 ((a:xs):xss) = "  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n8 ║"++show a++" "   ++ printWhiteBoard' 2 8 (xs:xss)
printWhiteBoard' 1 y ((a:xs):xss) = show y++" ║"++show a++" "                         ++ printWhiteBoard' 2 y (xs:xss)
printWhiteBoard' x y ((a:xs):xss) =  "║" ++ show a ++" "                              ++ printWhiteBoard' (x+1) y (xs:xss)
printWhiteBoard' _ 1 ([]:xs)      =  "║\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n"               ++ printWhiteBoard' 1 1 xs
printWhiteBoard' x y ([]:xs)      =  "║\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n"               ++ printWhiteBoard' 1 (y-1) xs
printWhiteBoard' x y []           =  "   a  b  c  d  e  f  g  h"

printBlackBoard' :: Int -> Int -> Board -> String 
printBlackBoard' 1 8 ((a:xs):xss) = printBlackBoard' 2 8 (xs:xss) ++ ("║" ++ show a++" ║" ++  "\n  ╚══╩══╩══╩══╩══╩══╩══╩══╝\n    h  g  f  e  d  c  b  a\n") 
printBlackBoard' 1 y ((a:xs):xss) = printBlackBoard' 2 y (xs:xss) ++ ("║"++show a++" ║ ")                       
printBlackBoard' x y ((a:xs):xss) = printBlackBoard' (x+1) y (xs:xss) ++ ("║"++show a++" ")                                
printBlackBoard' _ 1 ([]:xs)      = printBlackBoard' 1 1 xs ++ "  ╔══╦══╦══╦══╦══╦══╦══╦══╗\n1 "               
printBlackBoard' x y ([]:xs)      = printBlackBoard' 1 (y-1) xs ++ "\n  ╠══╬══╬══╬══╬══╬══╬══╬══╣\n" ++ show y ++ " "           
printBlackBoard' x y []           =  ""


{-changeSquare coordinate board -}
changeSquare :: Coordinate -> Board -> Square -> Board
changeSquare (x,0) (a:xs) square = changeSquare' x a square:xs
changeSquare (x,y) (a:xs) square = a : changeSquare (x,y-1) xs square

changeSquare' :: Int -> [Square] -> Square -> [Square]
changeSquare' 0 (a:xs) square = square:xs 
changeSquare' x (a:xs) square = a:changeSquare' (x-1) xs square

enPassant :: PColor -> Board -> Coordinate -> Coordinate -> Bool
enPassant clr brd crd1 crd2 = (isEmpty (getSquare crd2 brd) && (front || back)) && getSquare crd1 brd /= (Piece clr (Pawn DoubleMove )) && abs (snd crd1 - snd crd2) /= 2
    where front = case snd crd2 of
              0 -> getSquare crd2 brd == Piece (other clr) (Pawn DoubleMove)
              _ -> getSquare (fst crd2,snd crd2 - 1) brd == Piece (other clr) (Pawn DoubleMove)
          back = case snd crd2 of
              7 -> getSquare crd2 brd == Piece (other clr) (Pawn DoubleMove)
              _ -> getSquare (fst crd2,snd crd2 + 1) brd == Piece (other clr) (Pawn DoubleMove)

  
movePiece :: Board -> Coordinate -> Coordinate  -> IO Board
movePiece board crd1 crd2 = do
    let piece = getSquare crd1 board
        clr = getColor piece 
        newboard = changeSquare crd1 board Empty
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

removeDoublePawn :: PColor -> Board -> Board 
removeDoublePawn clr brd = if null doublepawn then brd else changeSquare (head doublepawn) brd Empty
    where doublepawn = filter (\x -> getSquare x brd == Piece (other clr) (Pawn DoubleMove)) [(x,y) | x <- [0..7], y <- [0..7]] 


resetDoubleMove :: PColor -> Board -> Board 
resetDoubleMove clr brd = if null doublepawn then brd else changeSquare (head doublepawn) brd (Piece clr (Pawn SingleMove))
    where doublepawn = filter (\x -> getSquare x brd == Piece clr (Pawn DoubleMove)) [(x,y) | x <- [0..7], y <- [0..7]] 

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

            
playerTurn :: Coordinate  -> Coordinate  -> PColor -> Board -> IO Board 
playerTurn crd1 crd2 clr brd = do
    if crd1 == (99,99)
        then kingSideCastle clr brd
        else if crd1 == (-99,-99)
            then queenSideCastle clr brd
            else do
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
        then case crd1 of
            "O-O"   -> return ((99,99),(99,99))
            "O-O-O" -> return ((-99,-99),(-99,-99))
            _       -> return (strToCoord crd1,strToCoord crd2)
        else do
            putStrLn "Either one or both inputs are not a valid coordinate"
            askMove

makeMove :: PColor -> Board -> IO Board
makeMove clr brd = do
        (crd1,crd2) <- askMove
        playerTurn crd1 crd2 clr brd
    

validInputs = [x:show y | x <- ['a'..'h'], y <- [1..8]] ++ ["O-O","O-O-O"]

validMove :: PColor -> PType -> Coordinate -> Coordinate -> Board -> IO Board 
validMove clr piece crd1 crd2 brd = do
        newbrd <- movePiece brd crd1 crd2
        let pieceMoves = case piece of
                (Pawn _) -> pawnMoves crd1 clr brd
                Knight -> horseMoves crd1 clr brd
                Bishop -> bishopmoves crd1 clr brd
                Queen -> queenmoves crd1 clr brd
                (Rook _) -> rookmoves crd1 clr brd
                (King _) -> kingmoves crd1 clr brd
        if crd2 `elem` pieceMoves        
            then if isChecked clr newbrd
                then do
                    putStrLn "Invalid Move"
                    makeMove clr brd
                else movePiece brd crd1 crd2
            else do 
                putStrLn "Invalid Move"
                makeMove clr brd

isMated :: PColor -> Board -> IO Bool
isMated clr brd = do
            brds <- mapM (\x -> case getType (getSquare x brd) of 
                        (Pawn _) -> mapM (movePiece brd x) (pawnMoves x clr brd)
                        Knight -> mapM (movePiece brd x) (horseMoves x clr brd)
                        Bishop -> mapM (movePiece brd x) (bishopmoves x clr brd)
                        Queen -> mapM (movePiece brd x) (queenmoves x clr brd)
                        (Rook _) -> mapM (movePiece brd x) (rookmoves x clr brd)
                        (King _) -> mapM (movePiece brd x) (kingmoves x clr brd))
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
                                                        (Rook _) -> Piece clr (Rook Moved) 
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

kingSideCastle :: PColor -> Board -> IO Board
kingSideCastle clr brd = do
    if isChecked clr brd 
        then do
            putStrLn "You can't castle, you are in check "
            makeMove clr brd 
        else 
            if canCastleK clr brd 
                then do
                    newbrd <- kCastle clr brd 
                    if isChecked clr newbrd 
                        then do
                            putStrLn "You can't castle, you will be in check "
                            makeMove clr brd
                        else return newbrd 
                else do
                        putStrLn "You can't castle, castling is blocked "
                        makeMove clr brd

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

testBoard :: Board
testBoard = [[Empty,Piece Black Knight,Piece Black Bishop,Piece Black (King Unmoved),Piece Black Queen ,Piece Black Bishop,Piece Black Knight,Piece Black (Rook Unmoved )],
             [Piece White (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),(Empty),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Piece White (Pawn SingleMove),Piece Black (Pawn DoubleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),(Empty),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove )],
             [Piece White (Rook Unmoved ),Piece White Knight,Piece White Bishop,Piece White Queen,Piece White (King Unmoved),Piece White Bishop,Piece White Knight,Piece White (Rook Unmoved )]]



testBoard2 :: Board
testBoard2 = [[Piece White (Pawn SingleMove),Piece Black Knight,Piece Black Bishop,Piece Black (King Unmoved),Piece Black Queen ,Piece Black Bishop,Piece Black Knight,Piece Black (Rook Unmoved )],
             [Piece White (Pawn SingleMove),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),(Empty),Piece Black (Pawn SingleMove ),Piece Black (Pawn SingleMove ),Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Piece White (Pawn SingleMove),Piece Black (Pawn DoubleMove)],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
             [Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),(Empty),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove ),Piece White (Pawn SingleMove )],
             [Piece White (Rook Unmoved ),Piece White Knight,Piece White Bishop,Piece White Queen,Piece White (King Unmoved),Piece White Bishop,Piece White Knight,Piece White (Rook Unmoved )]]


{-performTests = runTestTT $ TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16]

test1 = TestCase $ assertEqual "Looks if the safe Black king is in check on startboard" False (isChecked White initBoard)
test2 = TestCase $ assertEqual "Looks if the white king is in check on testboard" True (isChecked White testBoard)

test3 = TestCase $ assertEqual "Looks if the safe Black king is safe on testBoard" False (isChecked Black testBoard)

test4 = TestCase $ assertEqual "looks if squares is empty between king and rook, kingside for white  " False (canCastleK White initBoard)

test5 = TestCase $ assertEqual "looks if squares is empty between king and rook, kingside for Black " True (canCastleK Black castleBoard )

test6 = TestCase $ assertEqual "looks if squares is empty between king and rook, queenside for White " True (canCastleQ White castleBoard)

test7 = TestCase $ assertEqual "all possible moves for white at the start" [(0,5),(0,4),(1,5),(1,4),(2,5),(0,5),(2,5),(2,4),(3,5),(3,4),(4,5),(4,4),(5,5),(5,4),(6,5),(6,4),(7,5),(5,5),(7,5),(7,4)](possibleMoves White initBoard)

test8 = TestCase $ assertEqual "Checks if pawnMoves function finds all possible moves for the white pawn at coordinate (0,6)" [(0,5),(0,4)] (pawnMoves (0,6) White initBoard)

test9 = TestCase $ assertEqual "checks if promotedPawn function finds the correct promotedPawn on the testBoard" [] (getPromotedPawn White testBoard)

test10 = TestCase $ assertEqual "checks if promotedPawn function finds the correct promotedPawn on the testBoard" [] (getPromotedPawn Black testBoard)

test11 = TestCase $ assertEqual "Checks the White horse all possible moves on the startboard" [(2,5),(0,5)] (horseMoves (1,7) White initBoard)

test12 = TestCase $ assertEqual "finds the square the pawn can move to make the move enPassant" [(7,2)] (enPassantSquare (6,3) White testBoard)

test13 = TestCase $ assertEqual "Checks the White kings possible moves on the testBoard" [(4,6)] (kingmoves (4,7) White testBoard)

test14 = TestCase $ assertEqual "test the black kings possible moves on testBoard " [(4,1)] (kingmoves (3,0) Black testBoard)

test15 = TestCase $ assertEqual "test coordstr function for a number of coordinates" ["a8","b8","d5","e3","h1","g3"] (map coordToStr [(0,0),(1,0),(3,3),(4,5),(7,7),(6,5)])

test16 = TestCase $ assertEqual "test strTocord function for a number of stings" [(0,8),(1,1),(2,3),(1,5)] (map strToCoord ["a0", "b7","c5","b3"])-}