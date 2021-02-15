type Board = (Char, int, [Square])
Data PType = Bishop | Pawn | Rook | Knight | King | Queen
Data PColor = White | Black
type Piece = PColor PType