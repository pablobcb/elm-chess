module Model where

import Maybe       exposing (..)
import Maybe.Extra exposing (..)
import Dict        exposing (..)
import List        exposing (..)

{-------------------------- Color ---------------------------}
type Color = Black
           | White


other : Color -> Color
other color =
  case color of
    Black -> White
    White -> Black


{-------------------------- Piece ---------------------------}
type Figure = Pawn
            | Knight
            | Bishop
            | Rook
            | Queen
            | King


type alias Piece =
  { figure : Figure
  , moved  : Bool
  , color  : Color
  }


piece : Figure -> Color -> Piece
piece f c =
  { figure = f
  , color  = c
  , moved  = False
  }


{-------------------------- Board ---------------------------}
type alias Position = (Char, Int)


type alias Board = Dict Position (Maybe Piece)

getSquareContent : Board -> Position -> Maybe Piece
getSquareContent board =
  Maybe.Extra.join << (flip Dict.get) board


emptyRow : List (Maybe a)
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = repeat 8
        <| Just
        <| piece Pawn pawnColor


      makePiece pieceColor figure =
          Just <| piece figure pieceColor


      makeFirstRow color = List.map
        (makePiece color)
        [ Rook, Knight, Bishop , Queen
        , King, Bishop, Knight, Rook
        ]


      zip = List.map2 (,)


      makeRow number = zip
        ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
        (List.repeat 8 number)


  in Dict.fromList <| zip (makeRow 8) (makeFirstRow Black)
                   ++ zip (makeRow 7) (pawnRow Black)
                   ++ zip (makeRow 6) emptyRow
                   ++ zip (makeRow 5) emptyRow
                   ++ zip (makeRow 4) emptyRow
                   ++ zip (makeRow 3) emptyRow
                   ++ zip (makeRow 2) (pawnRow White)
                   ++ zip (makeRow 1) (makeFirstRow White)


{-------------------------- Game -------------------------}
type alias Graveyard = List (Maybe Figure)


type alias Player =
  { color     : Color
  , graveyard : Graveyard
  }


type State = Origin
           | Destination Position
           | Promotion Position
           | Finished

type alias Game =
  { board   : Board
  , player1 : Player
  , player2 : Player
  , turn    : Color
  , state   : State
  }


player : Color -> Player
player color =
  { color     = color
  --, graveyard = emptyRow ++ emptyRow
  , graveyard = repeat 16 <| Just Pawn
  }


makeInitialGame : Game
makeInitialGame =
  { board   = makeInitialBoard
  , player1 = (player Black)
  , player2 = (player White)
  , turn    = White
  , state   = Origin
  }
