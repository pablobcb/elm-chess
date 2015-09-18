module Model where

import Maybe exposing (..)
import Dict exposing (..)
import List exposing (..)


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
type alias Position = (Char, Char)


type alias Board = Dict Position (Maybe Piece)


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


  in Dict.fromList <| zip (makeRow '8') (makeFirstRow Black)
                   ++ zip (makeRow '7') (pawnRow Black)
                   ++ zip (makeRow '6') emptyRow
                   ++ zip (makeRow '5') emptyRow
                   ++ zip (makeRow '4') emptyRow
                   ++ zip (makeRow '3') emptyRow
                   ++ zip (makeRow '2') (pawnRow White)
                   ++ zip (makeRow '1') (makeFirstRow White)


{-------------------------- Game -------------------------}
type alias Graveyard = List (Maybe Figure)


type alias Player =
  { color     : Color
  , graveyard : Graveyard
  }


--type Status = Waiting Action Player -- Play, Promotion
--            | Finished Winner


type alias Game =
  { board   : Board
  , player1 : Player
  , player2 : Player
  }


player : Color -> Player
player color = 
  { color     = color
  , graveyard = repeat 16 <| Just Pawn
--  , graveyard = emptyRow ++ emptyRow
  }


game : Board -> Player -> Player -> Game
game board p1 p2 =
  { board   = board
  , player1 = p1
  , player2 = p2
  }


makeInitialGame : Game
makeInitialGame =
  game makeInitialBoard
       (player Black)
       (player White)
