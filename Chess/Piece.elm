module Chess.Piece where

import Chess.Color exposing (..)

type Figure = Pawn
            | Knight
            | Bishop
            | Rook
            | Queen
            | King


type alias Range = (Int, Int)


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

ranges : Piece -> List Range
ranges piece =
  let
    zip = List.map2 (,)

    zeros = List.repeat 7 0

    rookRanges =
      zip [ 1  .. 7 ] zeros ++
      zip [-1 .. -7 ] zeros ++
      zip zeros [ 1 ..  7 ] ++
      zip zeros [-1 .. -7 ]

    bishopRanges =
      zip [  1 ..  7 ][  1 ..  7 ] ++
      zip [ -1 .. -7 ][  1 ..  7 ] ++
      zip [  1 ..  7 ][ -1 .. -7 ] ++
      zip [ -1 .. -7 ][ -1 .. -7 ]

  in
    case piece.figure of
      Pawn ->
        case piece.color of
          White ->
            [(0, 1)] ++
              if piece.moved
              then []
              else [(0, 2)]

          Black ->
            [(0, -1)] ++
              if piece.moved
              then []
              else [(0, -2)]

      Rook ->
        rookRanges

      Bishop ->
        bishopRanges

      Knight ->
        [ (  1,  2 )
        , ( -1,  2 )
        , (  2,  1 )
        , (  2, -1 )
        , (  1, -2 )
        , ( -1, -2 )
        , ( -2,  1 )
        , ( -2, -1 )
        ]

      King ->
        [ (  0,  1 )
        , (  1,  1 )
        , (  1,  0 )
        , (  1, -1 )
        , (  0, -1 )
        , ( -1, -1 )
        , ( -1,  0 )
        , ( -1,  1 )
        ]

      Queen ->
        bishopRanges ++
          rookRanges


