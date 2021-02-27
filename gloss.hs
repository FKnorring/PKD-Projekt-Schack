import Board
import Moves
import Chess hiding (main)
import Graphics.Gloss
main = display (InWindow "Chesskell" (640, 640) (10, 10)) white (Circle 80)