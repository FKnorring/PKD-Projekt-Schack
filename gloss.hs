import Board
import Moves
import Chess hiding (main, play)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data State = Running | GameOver (Maybe PColor) deriving (Eq,Show)

data Game = Game { gameBoard :: Board,
                   gamePlayer :: PColor,
                   gameState :: State
                    } deriving (Eq, Show)

window =  InWindow "Chesskell" (480, 480) (100, 100)

initGame = Game { gameBoard = initBoard, gamePlayer = White, gameState = Running}

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
    let pieces = [whitepawn, whitebishop,whiterook,whiteknight,whiteking,whitequeen,blackpawn,blackbishop,blackrook,blackknight,blackking,blackqueen,empty]
    play window white 30 initGame (renderBoard pieces) getClick (const id)

renderGame :: Game -> Picture 
renderGame game =
    case gameState game of 
        Running -> renderBoard (gameBoard game)
        GameOver winner -> gameOverBoard winner (gameBoard game)

renderBoard :: Board -> [Picture] -> Picture
renderBoard imgs brd =
 pictures $ [translate (fromIntegral $ -210+60*x) (fromIntegral $ 210-60*y) $
        case sqr of
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


getfstClick :: Event -> Game -> Game
getfstClick (EventKey (MouseButton LeftButton) Down _ (x, y)) game = 
    case gameState game of 
        Running -> playerTurn' game $ parseCoord (x,y)
        _ -> game
getfstClick _ game = game

getSndClick :: Event -> Coordinate -> Game -> Game
getSndClick (EventKey (MouseButton LeftButton) Down _ (x, y)) crd1 game = playerTurn'' game crd1 $ parseCoord (x,y)

parseCoord :: (Float,Float) -> Coordinate
parseCoord (x,y) = (floor (x + 210)/60,floor (y + 210)/60)

playerTurn' :: Game -> Coordinate -> Game
playerTurn' game cord = case getSquare cord brd of
        Empty -> getfstClick game
        (Piece (other clr) _ ) -> getfstClick game
        _ -> getSndClick cord game
    where brd = gameBoard game
          clr = gamePlayer game

playerTurn'' :: Game -> Coordinate -> Coordinate -> Game
playerTurn'' game crd1 crd2 = gameBoard 


move = undefined