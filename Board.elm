module Board where

import Maybe exposing (..)
import Dict
import List exposing (..)


type Color = Black 
           | White

other : Color -> Color
other color =
  case color of
    Black -> White
    White -> Black

type alias Player = { color : Color }


type Piece = Pawn 
           | Knight
           | Bishop 
           | Rook 
           | Queen 
           | King 


type alias PieceInstance = 
    { piece : Piece
    , moved : Bool
    , color : Color
    }

pieceInstance : Piece -> Color -> Bool -> PieceInstance
pieceInstance p c m = { piece = p, moved = m, color = c }


type Board = Dict String (Maybe PieceInstance)

--type alias Game = {
--  board : Board,
--  p1: Player,
--  p2: Player,
--  p1Graveyard : List Piece,
--  p2Graveyard : List Piece
--}

zip = List.map2 (,)

makeInitialBoard =
  let pawnRow color= repeat 8 <| Just <| pieceInstance Pawn color False
      emptyRow = repeat 8 Nothing
  in 
    Dict.fromList  <| [
      ( "A8", Just <| pieceInstance Rook Black False )
    , ( "B8", Just <| pieceInstance Knight Black False )
    , ( "C8", Just <| pieceInstance Bishop Black False )
    , ( "D8", Just <| pieceInstance King Black False )
    , ( "E8", Just <| pieceInstance Queen Black False )
    , ( "F8", Just <| pieceInstance Bishop Black False )
    , ( "G8", Just <| pieceInstance Knight Black False )
    , ( "H8", Just <| pieceInstance Rook Black False )
    ] 
    ++ zip ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"] (pawnRow Black)
    ++ zip ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"] emptyRow
    ++ zip ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"] emptyRow
    ++ zip ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"] emptyRow
    ++ zip ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"] emptyRow
    ++ zip ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"] (pawnRow White) ++
    [ ( "A1", Just <| pieceInstance Rook White False )
    , ( "B1", Just <| pieceInstance Knight White False )
    , ( "C1", Just <| pieceInstance Bishop White False )
    , ( "D1", Just <| pieceInstance Queen White False )
    , ( "E1", Just <| pieceInstance King White False )
    , ( "F1", Just <| pieceInstance Bishop White False )
    , ( "G1", Just <| pieceInstance Knight White False )
    , ( "H1", Just <| pieceInstance Rook White False )
    ] 
  --, fromList [ makeSquare Black, Just Pawn
  --  , makeSquare White, Just Pawn
  --  , makeSquare Black, Just Pawn
  --  , makeSquare White, Just Pawn
  --  , makeSquare Black, Just Pawn
  --  , makeSquare White, Just Pawn
  --  , makeSquare Black, Just Pawn
  --  , makeSquare White, Just Pawn
  --  ]
  --]
