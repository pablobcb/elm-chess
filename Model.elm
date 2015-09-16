module Model where

import Maybe exposing (..)
import Dict exposing (..)
import List exposing (..)


type Color = Black 
           | White

other : Color -> Color
other color =
  case color of
    Black -> White
    White -> Black

type alias Player = { color : Color }


type Figure = Pawn 
            | Knight
            | Bishop 
            | Rook 
            | Queen 
            | King 


type alias Piece = { figure : Figure , moved  : Bool , color  : Color }

piece : Figure -> Color -> Bool -> Piece
piece f c m = { figure = f, moved = m, color = c }


type alias Square = { piece : Maybe Piece , color : Color }

square : Color -> Maybe Piece -> Square
square c p  = { piece = p, color = c }


type alias Board = Dict String Square

--type alias Game = {
--  board : Board,
--  p1: Player,
--  p2: Player,
--  p1Graveyard : List Piece,
--  p2Graveyard : List Piece
--}

zip = List.map2 (,)

makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor firstSquareColor = List.map2 square
        (concat <| repeat 4 [firstSquareColor, other firstSquareColor]) 
        (repeat 8 <| Just <| piece Pawn pawnColor False)

      emptyRow color = List.map2 square 
        (concat <| repeat 4 [color, other color])
        (repeat 8 Nothing)

  in Dict.fromList <| [
      ( "A8", square Black <| Just <| piece Rook   Black False )
    , ( "B8", square White <| Just <| piece Knight Black False )
    , ( "C8", square Black <| Just <| piece Bishop Black False )
    , ( "D8", square White <| Just <| piece King   Black False )
    , ( "E8", square Black <| Just <| piece Queen  Black False )
    , ( "F8", square White <| Just <| piece Bishop Black False )
    , ( "G8", square Black <| Just <| piece Knight Black False )
    , ( "H8", square White <| Just <| piece Rook   Black False )
    ] 
    ++ zip ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"] (pawnRow Black White)
    ++ zip ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"] (emptyRow White)
    ++ zip ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"] (emptyRow Black)
    ++ zip ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"] (emptyRow White)
    ++ zip ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"] (emptyRow Black)
    ++ zip ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"] (pawnRow White White) ++
    [ ( "A1", square Black <| Just <| piece Rook   White False )
    , ( "B1", square White <| Just <| piece Knight White False )
    , ( "C1", square Black <| Just <| piece Bishop White False )
    , ( "D1", square White <| Just <| piece Queen  White False )
    , ( "E1", square Black <| Just <| piece King   White False )
    , ( "F1", square White <| Just <| piece Bishop White False )
    , ( "G1", square Black <| Just <| piece Knight White False )
    , ( "H1", square White <| Just <| piece Rook   White False )
    ] 
