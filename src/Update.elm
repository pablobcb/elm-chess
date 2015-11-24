module Update where

import Debug exposing (..)

import Chess.Game  as Game exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece as Game exposing (..)


type Action
  = UpdateTimer
  | Promote Position Figure
  | Click Position


update : Action -> Game -> Game
update action game =
  case action of
    -- increase turn timer by 1 second
    UpdateTimer ->
      Game.tick game


    -- sets promoted piece into position on board
    Promote promotedPiecePosition figure ->
      Game.promotePiece game promotedPiecePosition figure


    -- Position of a click on the board
    Click selectedPosition ->
      Game.handleClick game selectedPosition
