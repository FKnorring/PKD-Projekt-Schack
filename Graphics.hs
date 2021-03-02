import Board
import Moves
import Chess hiding (main, play)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data State = Crd1 | Crd2 | Mate | Stalemate deriving (Eq,Show)

type Coords = (Coordinate,Coordinate)

data Game = Game { gameBoard :: Board,
                   gamePlayer :: PColor,
                   gameState :: State,
                   fstCrd :: Coordinate
                    } deriving (Eq, Show)

window =  InWindow "Chesskell" (480, 480) (100, 100)

initGame = Game { gameBoard = initBoard, gamePlayer = White, gameState = Crd1, fstCrd = (0,0)}

main :: IO ()
main = do
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
    playIO window white 30 initGame (renderGame pieces) getClicks bsFunc

bsFunc :: Float -> Game -> IO Game
bsFunc _ = return

renderGame :: [Picture] -> Game -> IO Picture 
renderGame imgs game = case gameState game of
    Mate | Stalemate -> return $ scale (0.2) (0.2) $ Text $ show (gamePlayer game) ++ " is mated"
    _ -> return $ renderBoard imgs (gameBoard game)
       

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


getClicks :: Event -> Game -> IO Game
getClicks (EventKey (MouseButton LeftButton) Down _ (x,y)) game = do
    mated <- isMated (gamePlayer game) (gameBoard game)
    if mated
    then do return $ game {gameState = Mate | Stalemate}
    else case gameState game of
        Crd1 -> do
            let crd1 = parseCoord (x,y)
                brd = gameBoard game
                clr = gamePlayer game
                sqr = getSquare crd1 brd
            if isEmpty sqr
                then do
                    putStrLn "No piece at that square!"
                    return game
                else if getColor sqr == clr
                    then return $ game {gameState = Crd2, fstCrd = crd1}
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

parseCoord :: (Float,Float) -> Coordinate
parseCoord (x,y) = (floor ((x + 240)/60),floor ((240 - y)/60))

validMoveGame :: Coordinate -> Coordinate -> Game -> IO Game 
validMoveGame crd1 crd2 game = do
        let brd = gameBoard game
            clr = gamePlayer game
            piece = getType $ getSquare crd1 brd
            pieceMoves = case piece of
                    (Pawn _) -> pawnMoves crd1 clr brd
                    Knight -> horseMoves crd1 clr brd
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

autopromote :: PColor -> Board -> IO Board 
autopromote clr brd = do
            if null (getPromotedPawn clr brd)
                then return brd
                else do
                    return $ changeSquare (head (getPromotedPawn clr brd)) brd (Piece clr Queen)