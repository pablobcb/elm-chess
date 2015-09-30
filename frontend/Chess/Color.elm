module Chess.Color where

type Color = Black
           | White


other : Color -> Color
other color =
  case color of
    Black -> White
    White -> Black
