import Board
import Moves
import Chess hiding (main, play)
import Graphics.Gloss

window =  InWindow "Chesskell" (480, 480) (100, 100)

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
    let brd = initBoard
    display window white (render brd [whitepawn, whitebishop,whiterook,whiteknight,whiteking,whitequeen,blackpawn,blackbishop,blackrook,blackknight,blackking,blackqueen,empty])

render :: Board -> [Picture] -> Picture
render brd imgs =
 pictures $ [ translate (fromIntegral $ -210+60*x)
                (fromIntegral $ 210-60*y) $
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

checkCoordinate :: Float -> Maybe Int
checkCoordinate f' =
  let f = f' / 60
  in  (-1) <$ guard (-1.5 < f && f < -0.5)
  <|> 0    <$ guard (-0.5 < f && f < 0.5)
  <|> 1    <$ guard (0.5  < f && f < 1.5)

handleKeys :: Event -> Board -> Board
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
  fromMaybe b $ do
    x <- checkCoordinate x'
    y <- checkCoordinate y'
    return $ pushToken (x, y) b
handleKeys _ b = b