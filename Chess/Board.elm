module Chess.Board where

import Dict        exposing (..)
import Maybe       exposing (..)
import Maybe.Extra exposing (..)

import Chess.Color exposing (..)
import Chess.Piece exposing (..)

type alias Position = (Char, Int)

type alias Square = Maybe Piece

type alias Board = Dict Position Square

getSquareContent : Board -> Position -> Square
getSquareContent board =
  Maybe.Extra.join << (flip Dict.get) board


validateMove origin destination game =
  let
    otherColor =
      case getSquareContent game.board destination of
        Just piece ->
          piece.color /= game.turn

        Nothing ->
          True

  in
    (origin /= destination) -- a piece cant move to the same place
     && otherColor          -- a piece cant take an ally




emptyRow : List (Maybe a)
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = List.repeat 8
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

