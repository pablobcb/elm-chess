module Chess.Piece where

import Chess.Color exposing (..)

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

