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

      squareWith squareColor figure pieceColor =
        square squareColor << Just <| piece figure pieceColor False

  in Dict.fromList <| [
      ( "A8", squareWith Black Rook   Black )
    , ( "B8", squareWith White Knight Black )
    , ( "C8", squareWith Black Bishop Black )
    , ( "D8", squareWith White King   Black )
    , ( "E8", squareWith Black Queen  Black )
    , ( "F8", squareWith White Bishop Black )
    , ( "G8", squareWith Black Knight Black )
    , ( "H8", squareWith White Rook   Black )
    ] 
    ++ zip ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"] (pawnRow Black White)
    ++ zip ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"] (emptyRow White)
    ++ zip ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"] (emptyRow Black)
    ++ zip ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"] (emptyRow White)
    ++ zip ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"] (emptyRow Black)
    ++ zip ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"] (pawnRow White White) ++
    [ ( "A1", squareWith Black Rook   White )
    , ( "B1", squareWith White Knight White )
    , ( "C1", squareWith Black Bishop White )
    , ( "D1", squareWith White Queen  White )
    , ( "E1", squareWith Black King   White )
    , ( "F1", squareWith White Bishop White )
    , ( "G1", squareWith Black Knight White )
    , ( "H1", squareWith White Rook   White )
    ] 
