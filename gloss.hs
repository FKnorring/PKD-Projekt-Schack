import Board
import Moves
import Chess hiding (main, play)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data State = Crd1 | Crd2 | GameOver deriving (Eq,Show)

type Coords = (Coordinate,Coordinate)

data Game = Game { gameBoard :: Board,
                   gamePlayer :: PColor,
                   gameState :: State,
                   gameCoords :: Coords
                    } deriving (Eq, Show)

window =  InWindow "Chesskell" (480, 480) (100, 100)

initGame = Game { gameBoard = initBoard, gamePlayer = White, gameState = Crd1, gameCoords = ((0,0),(0,0))}

main :: IO ()
main = do
    whitepawn <- loadBMP "whitepawn.bmp"
    whitebishop <- loadBMP "whitebishop.bmp"
    whiterook <- loadBMP "whiterook.bmp"
    whiteknight <- loadBMP "whiteknight.bmp"
    whiteking <- loadBMP "whiteking.bmp"
    whitequeen <- loadBMP "whitequeen.bmp"
    blackpawn <- loadBMP "blackpawn.bmp"
    blackbishop <- loadBMP "blackbishop.bmp"
    blackrook <- loadBMP "blackrook.bmp"
    blackknight <- loadBMP "blackknight.bmp"
    blackking <- loadBMP "blackking.bmp"
    blackqueen <- loadBMP "blackqueen.bmp"
    empty <- loadBMP "empty.bmp"
    whitepawn' <- loadBMP "whitepawn'.bmp"
    whitebishop' <- loadBMP "whitebishop'.bmp"
    whiterook' <- loadBMP "whiterook'.bmp"
    whiteknight' <- loadBMP "whiteknight'.bmp"
    whiteking' <- loadBMP "whiteking'.bmp"
    whitequeen' <- loadBMP "whitequeen'.bmp"
    blackpawn' <- loadBMP "blackpawn'.bmp"
    blackbishop' <- loadBMP "blackbishop'.bmp"
    blackrook' <- loadBMP "blackrook'.bmp"
    blackknight' <- loadBMP "blackknight'.bmp"
    blackking' <- loadBMP "blackking'.bmp"
    blackqueen' <- loadBMP "blackqueen'.bmp"
    empty' <- loadBMP "empty'.bmp"
    let pieces = [whitepawn, whitebishop,whiterook,whiteknight,whiteking,whitequeen,blackpawn,blackbishop,blackrook,blackknight,blackking,blackqueen,empty,
                  whitepawn', whitebishop',whiterook',whiteknight',whiteking',whitequeen',blackpawn',blackbishop',blackrook',blackknight',blackking',blackqueen',empty']
    playIO window white 30 initGame (renderGame pieces) getClicks bsFunc

bsFunc :: Float -> Game -> IO Game
bsFunc _ game = return game

renderGame :: [Picture] -> Game -> IO Picture 
renderGame imgs game = case gameState game of
    GameOver -> return $ Text (show (gamePlayer game) ++ " is mated")
    _ -> return $ renderBoard imgs (gameBoard game)
       

renderBoard :: [Picture] -> Board -> Picture
renderBoard imgs brd =
 pictures $ [translate (fromIntegral $ -210+60*x) (fromIntegral $ 210-60*y) $
        case (even x, even y) of
        (True,True) -> case sqr of
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
        (False,False) -> case sqr of
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
        _ -> case sqr of
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
        | x <- [0..7], y <- [0..7], sqr <- [(brd !! y) !! x]]


getClicks :: Event -> Game -> IO Game
getClicks (EventKey (MouseButton LeftButton) Down _ (x,y)) game = do
    mated <- isMated (gamePlayer game) (gameBoard game)
    if trace (show mated) mated
    then do return $ game {gameState = GameOver}
    else case gameState game of
        Crd1 -> do
            let crd1 = parseCoord (x,y)
                brd = gameBoard game
                clr = gamePlayer game
                sqr = getSquare crd1 brd
            if isEmpty sqr
                then return game
                else if getColor sqr == clr
                    then trace (show crd1) return $ game {gameState = Crd2, gameCoords = (crd1,(0,0))}
                    else return game
        Crd2 -> do 
            let brd = gameBoard game
                crd1 = fst $ gameCoords game
                crd2 = parseCoord (x,y)
                clr = gamePlayer game
            case clr of
                White -> case (crd1,crd2) of
                    ((4,7),(6,7)) -> if canCastleK clr brd 
                        then do
                        newbrd <- kCastle clr brd
                        if isChecked clr newbrd 
                            then return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}
                            else return $ game {gameBoard = newbrd, gamePlayer = other clr, gameState = Crd1, gameCoords = ((9,9),(9,9))}
                        else validMoveGame crd1 crd2 game
                    ((4,7),(2,7)) -> if canCastleQ clr brd
                        then do
                        newbrd <- qCastle clr brd
                        if isChecked clr newbrd 
                            then return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}
                            else return $ game {gameBoard = newbrd, gamePlayer = other clr, gameState = Crd1, gameCoords = ((9,9),(9,9))}
                        else validMoveGame crd1 crd2 game
                    _ -> validMoveGame crd1 crd2 game
                Black -> case (crd1,crd2) of
                    ((4,0),(6,0)) -> if canCastleK clr brd 
                        then do
                        newbrd <- kCastle clr brd
                        if isChecked clr newbrd 
                            then return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}
                            else return $ game {gameBoard = newbrd, gamePlayer = other clr, gameState = Crd1, gameCoords = ((9,9),(9,9))}
                        else validMoveGame crd1 crd2 game
                    ((4,0),(2,0)) -> if canCastleQ clr brd
                        then do
                        newbrd <- qCastle clr brd
                        if isChecked clr newbrd 
                            then return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}
                            else return $ game {gameBoard = newbrd, gamePlayer = other clr, gameState = Crd1, gameCoords = ((9,9),(9,9))}
                        else validMoveGame crd1 crd2 game
                    _ -> validMoveGame crd1 crd2 game
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
                    (King _) -> kingmoves crd1 clr brd
        newbrd <- movePiece brd crd1 crd2
        if crd2 `elem` pieceMoves        
            then if isChecked clr newbrd
                then return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}
                else do
                    return $ game { gameBoard = newbrd, gamePlayer = other clr, gameState = Crd1}
            else return $ game {gameState = Crd1, gameCoords = ((9,9),(9,9))}

move = undefined