module Chess.Piece where

import Debug exposing (..)

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

-- creates piece
piece : Figure -> Color -> Piece
piece f c =
  { figure = f
  , color  = c
  , moved  = False
  }


-- because pawns take pieces in a different
-- way from how they move, this function is necessary
pawnTakeRanges : Color -> { left : Range, right : Range }
pawnTakeRanges color =
  case color of
     White ->
       { right = (1, 1), left = (-1, 1) }

     Black ->
       { right = (1, -1), left = (-1, -1) }


-- movement ranges for each piece
ranges : Piece -> List Range
ranges piece =
  let
    zip = List.map2 (,)

    zeros = List.repeat 7 0

    oneToSeven = [ 1 .. 7 ]

    negativeOneToSeven =
      List.map ( (*) (-1)) oneToSeven

    rookRanges =
      zip oneToSeven zeros ++
      zip negativeOneToSeven zeros ++
      zip zeros oneToSeven ++
      zip zeros negativeOneToSeven

    bishopRanges =
      zip oneToSeven oneToSeven ++
      zip negativeOneToSeven oneToSeven ++
      zip oneToSeven negativeOneToSeven ++
      zip negativeOneToSeven negativeOneToSeven

    kingRanges =
      [ (  0,  1 )
      , (  1,  1 )
      , (  1,  0 )
      , (  1, -1 )
      , (  0, -1 )
      , ( -1, -1 )
      , ( -1,  0 )
      , ( -1,  1 )
      ]

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
        watch "bishop Ranges" bishopRanges

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
        kingRanges

      Queen ->
        bishopRanges
        ++ rookRanges
        ++ kingRanges


