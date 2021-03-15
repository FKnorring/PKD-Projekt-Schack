import Board
import Moves
import Chess hiding (main, play)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

{-data State represents the state of the game.
    If the state is either Crd1 or Crd2 the game is running
    and awaiting the first coordinate if state is Crd1,
    the second if the state is Crd2
    Mate and Stalemate is the game over state of the game-}
data State = Crd1 | Crd2 | Mate | Stalemate deriving (Eq,Show)

{-data Game is a record containing information about the game.
    gameBoard represents the board that is currently being played,
    gamePlayer represents who is playing,
    gameState represents what state the game is in,
    fstCrd is a global variable to store the first coordinate the player clicked on-}
data Game = Game { gameBoard :: Board,
                   gamePlayer :: PColor,
                   gameState :: State,
                   fstCrd :: Coordinate
                    } deriving (Eq, Show)

--defines window size and name
window =  InWindow "Chesskell" (480, 480) (100, 100)

--initial state of game
initGame = Game { gameBoard = initBoard, gamePlayer = White, gameState = Crd1, fstCrd = (0,0)}

{-main
    a function to run the game
    SIDE EFFECTS: Opens a separate window
-}
main :: IO ()
main = do
    playIO window white 30 initGame renderGame getClicks (\_ -> return)

{-renderGame game
    a function to render the Board
    RETURNS: an I/O Picture action containing the picture of the board if the game is running
             an I/O Picture action containing the losing screen if the game is over
-}
renderGame :: Game -> IO Picture 
renderGame game = case gameState game of
    Mate -> return $ scale 0.2 0.2 $ Text $ show (gamePlayer game) ++ " is mated"
    Stalemate -> return $ scale 0.2 0.2 $ Text "Stalemate"
    _ -> do
    whitepawn <- loadBMP "imgs/whitepawn.bmp"
    whitebishop <- loadBMP "imgs/whitebishop.bmp"
    whiterook <- loadBMP "imgs/whiterook.bmp"
    whiteknight <- loadBMP "imgs/whiteknight.bmp"
    whiteking <- loadBMP "imgs/whiteking.bmp"
    whitequeen <- loadBMP "imgs/whitequeen.bmp"
    blackpawn <- loadBMP "imgs/blackpawn.bmp"
    blackbishop <- loadBMP "imgs/blackbishop.bmp"
    blackrook <- loadBMP "imgs/blackrook.bmp"
    blackknight <- loadBMP "imgs/blackknight.bmp"
    blackking <- loadBMP "imgs/blackking.bmp"
    blackqueen <- loadBMP "imgs/blackqueen.bmp"
    empty <- loadBMP "imgs/empty.bmp"
    whitepawn' <- loadBMP "imgs/whitepawn'.bmp"
    whitebishop' <- loadBMP "imgs/whitebishop'.bmp"
    whiterook' <- loadBMP "imgs/whiterook'.bmp"
    whiteknight' <- loadBMP "imgs/whiteknight'.bmp"
    whiteking' <- loadBMP "imgs/whiteking'.bmp"
    whitequeen' <- loadBMP "imgs/whitequeen'.bmp"
    blackpawn' <- loadBMP "imgs/blackpawn'.bmp"
    blackbishop' <- loadBMP "imgs/blackbishop'.bmp"
    blackrook' <- loadBMP "imgs/blackrook'.bmp"
    blackknight' <- loadBMP "imgs/blackknight'.bmp"
    blackking' <- loadBMP "imgs/blackking'.bmp"
    blackqueen' <- loadBMP "imgs/blackqueen'.bmp"
    empty' <- loadBMP "imgs/empty'.bmp"
    let pieces = [whitepawn,whitebishop,whiterook,whiteknight,whiteking,whitequeen,blackpawn,blackbishop,blackrook,blackknight,blackking,blackqueen,empty,
                  whitepawn',whitebishop',whiterook',whiteknight',whiteking',whitequeen',blackpawn',blackbishop',blackrook',blackknight',blackking',blackqueen',empty']
    return $ renderBoard pieces (gameBoard game)
       
{-renderBoard images board
    a function to create a picture of the board from the correct list of images
    PRE: images has to be a list of all bitmap pictures in the right order
    RETURNS: a picture of the board built with the list of images
-}
renderBoard :: [Picture] -> Board -> Picture
renderBoard imgs brd =
 pictures $ [translate (fromIntegral $ -210+60*x) (fromIntegral $ 210-60*y) $
        if even x /= even y 
            then case sqr of
                Piece White (Pawn _) -> imgs !! 13
                Piece White Bishop -> imgs !! 14
                Piece White (Rook _) -> imgs !! 15
                Piece White Knight -> imgs !! 16
                Piece White (King _) -> imgs !! 17
                Piece White Queen -> imgs !! 18
                Piece Black (Pawn _) -> imgs !! 19
                Piece Black Bishop -> imgs !! 20
                Piece Black (Rook _) -> imgs !! 21
                Piece Black Knight -> imgs !! 22
                Piece Black (King _) -> imgs !! 23
                Piece Black Queen -> imgs !! 24
                Empty -> imgs !! 25
            else case sqr of
                Piece White (Pawn _) -> imgs !! 0
                Piece White Bishop -> imgs !! 1
                Piece White (Rook _) -> imgs !! 2
                Piece White Knight -> imgs !! 3
                Piece White (King _) -> imgs !! 4
                Piece White Queen -> imgs !! 5
                Piece Black (Pawn _) -> imgs !! 6
                Piece Black Bishop -> imgs !! 7
                Piece Black (Rook _) -> imgs !! 8
                Piece Black Knight -> imgs !! 9
                Piece Black (King _) -> imgs !! 10
                Piece Black Queen -> imgs !! 11
                Empty -> imgs !! 12
        | x <- [0..7], y <- [0..7], sqr <- [(brd !! y) !! x]]


{-getClicks event game
    a function to handle the mouseclicks in the window and make moves on the board
    RETURNS: an I/O Game action
    SIDE EFFECTS: Prints text to terminal
-}
getClicks :: Event -> Game -> IO Game
getClicks (EventKey (MouseButton LeftButton) Down _ (x,y)) game = do
    mated <- hasNoValidMoves (gamePlayer game) (gameBoard game)
    if mated
    then if isChecked (gamePlayer game) (gameBoard game) then return $ game {gameState = Mate}
                              else return $ game {gameState = Stalemate}
    else case gameState game of
        Crd1 -> do
            let crd1 = parseCoord (x,y)
                clr = gamePlayer game
                brd = resetDoubleMove clr (gameBoard game)
                sqr = getSquare crd1 brd
            if isEmpty sqr
                then do
                    putStrLn "No piece at that square!"
                    return game
                else if getColor sqr == clr
                    then return $ game {gameBoard = brd, gameState = Crd2, fstCrd = crd1}
                    else do
                        putStrLn $ "No " ++ show clr ++ " piece at that square!"
                        return game
        Crd2 -> do 
            let brd = gameBoard game
                crd1 = fstCrd game
                crd2 = parseCoord (x,y)
                clr = gamePlayer game
            validMoveGame crd1 crd2 game
getClicks _ game = return game


{-parseCoord windowcoordinate
    a function to translate the coordinates on the window to a coordinate on the chessboard
    PRE: -240 < x < 240
         -240 < y < 240
    RETURNS: a coordinate from the coordinates on the window
    EXAMPLES: parseCoord (239.3,145.6)  == (7,1)
              parseCoord (-239.3,145.6) == (0,1)
-}
parseCoord :: (Float,Float) -> Coordinate
parseCoord (x,y) = (floor ((x + 240)/60),floor ((240 - y)/60))

{-validMoveGame firstcoordinate secondcoordinate game
    a function to update the gameboard with a new move if the move from the first to the second coordinate is valid
    RETURNS: an I/O Game action where the gameBoard is updated, gamePlayer is switched and gameState is Crd1 if a move is made
             an I/O Game action where the gameBoard is updated, gamePlayer is the same and gameState is Crd1 if no move is made
    SIDE EFFECTS: Prints text to terminal
-}
validMoveGame :: Coordinate -> Coordinate -> Game -> IO Game 
validMoveGame crd1 crd2 game = do
        let brd = gameBoard game
            clr = gamePlayer game
            piece = getType $ getSquare crd1 brd
            pieceMoves = case piece of
                    (Pawn _) -> pawnMoves crd1 clr brd
                    Knight -> knightmoves crd1 clr brd
                    Bishop -> bishopmoves crd1 clr brd
                    Queen -> queenmoves crd1 clr brd
                    (Rook _) -> rookmoves crd1 clr brd
                    (King _) -> kingmoves crd1 clr brd ++ castlemoves clr brd
        newbrd <- movePiece brd crd1 crd2
        if crd2 `elem` pieceMoves   
            then if isChecked clr newbrd
                then do
                    putStrLn "Invalid Move!"
                    return $ game {gameState = Crd1, fstCrd = (9,9)}
                else do
                    newbrd' <- autopromote clr newbrd
                    return $ game { gameBoard = newbrd', gamePlayer = other clr, gameState = Crd1}
            else do
                putStrLn "Invalid move!"
                return $ game {gameState = Crd1, fstCrd = (9,9)}

{-autopromote clr brd
    a function to promote a pawn to a queen if there is a pawn that has reached the other side
    RETURNS: an I/O Board action where the pawn has been promoted to a queen if there was a pawn that could be promote
             an I/O Board action with the same board if no pawn has been promoted
-}
autopromote :: PColor -> Board -> IO Board 
autopromote clr brd = do
            if null (getPromotedPawn clr brd)
                then return brd
                else do
                    return $ changeSquare (head (getPromotedPawn clr brd)) brd (Piece clr Queen)