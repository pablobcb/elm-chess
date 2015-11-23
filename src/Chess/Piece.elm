module Chess.Piece where

import Debug exposing (..)

import Chess.Color exposing (..)

type Figure
  = Pawn
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
