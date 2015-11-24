module View.Piece where

import String          exposing (..)
import Chess.Piece     exposing (..)

getPieceClass : Piece -> String
getPieceClass piece =
  toLower <| String.join " "
    [ "piece"
    , toString piece.figure
    , toString piece.color
    ]
