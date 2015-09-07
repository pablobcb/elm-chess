import Maybe exposing ( Maybe(..) )
import Array exposing ( Array(..) )


type Color = Black 
           | White


type alias Player = Color


type Piece = Pawn 
           | Knight 
           | Bishop 
           | Rook 
           | Queen 
           | King


type alias PieceInstance = 
    { piece : Piece
    , color : Color
    , moved : Bool
    }


type alias Square = 
    { color : Color
    , piece : Maybe PieceInstance
    ,
    }


type alias Row = Array Square

type alias Board = Array Row