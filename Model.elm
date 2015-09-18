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


piece : Figure -> Color -> Bool -> Piece
piece f c m = 
  { figure = f
  , moved  = m
  , color  = c
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
        <| piece Pawn pawnColor False

      makePiece pieceColor figure =
          Just <| piece figure pieceColor False

      firstRow = [ Rook, Knight, Bishop
                 , Queen, King, Bishop, Knight, Rook
                 ]

      zip = List.map2 (,)

      makeRow number = zip 
        ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
        (List.repeat 8 number)


  in Dict.fromList <|
    zip (makeRow '8')
        (List.map (makePiece Black)  firstRow)
    ++
    zip (makeRow '7')
        (pawnRow Black)
    ++
    zip (makeRow '6')
        emptyRow
    ++
    zip (makeRow '5')
        emptyRow
    ++
    zip (makeRow '4')
        emptyRow
    ++
    zip (makeRow '3')
        emptyRow
    ++
    zip (makeRow '2')
        (pawnRow White)
    ++
    zip (makeRow '1')
        (List.map (makePiece White) firstRow)


{-------------------------- Game -------------------------}
type alias Graveyard = List (Maybe Figure)


type alias Player =
  { color     : Color
  , graveyard : Graveyard
  }


type alias Game =
  { board   : Board
  , player1 : Player
  , player2 : Player
  }


player : Color -> Player
player color = 
  { color     = color
  , graveyard = emptyRow ++ emptyRow
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
