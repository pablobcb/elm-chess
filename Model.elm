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

type alias Position = String

type Figure = Pawn 
            | Knight
            | Bishop
            | Rook
            | Queen
            | King


type alias Piece = { figure : Figure , moved  : Bool , color  : Color }

piece : Figure -> Color -> Bool -> Piece
piece f c m = { figure = f, moved = m, color = c }


type alias Board = Dict Position (Maybe Piece)


--type alias Game = {
--  board : Board,
--  p1: Player,
--  p2: Player,
--  p1Graveyard : List Piece,
--  p2Graveyard : List Piece }


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = repeat 8 <| Just <| piece Pawn pawnColor False

      emptyRow = repeat 8 Nothing

      makePiece pieceColor figure =
          Just <| piece figure pieceColor False

      firstRow = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

      zip = List.map2 (,)

  in Dict.fromList <|
    zip ["A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8"]
        (List.map (makePiece Black)  firstRow)        ++

    zip ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"]
        (pawnRow Black)                               ++

    zip ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"]
        (emptyRow )                              ++
    
    zip ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"]
        (emptyRow )                              ++

    zip ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"]
        (emptyRow )                              ++
    
    zip ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"]
        (emptyRow )                              ++

    zip ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"]
        (pawnRow White)                               ++

    zip ["A1", "B1",  "C1","D1", "E1", "F1", "G1", "H1"]
        (List.map (makePiece White) firstRow)
