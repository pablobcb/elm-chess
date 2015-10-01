module Update where

import Debug exposing (..)

import Dict exposing (..)

import Chess.Game  as Game exposing (..)
import Chess.Color as Game exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece as Game exposing (..)


-- tentar tirar tudo que seta o estado do jogo da update

type Action
  = UpdateTimer
  | Restart
  | Promote Position Figure
  | Click Position


update : Action -> Game -> Game
update action game =
  case action of

    UpdateTimer ->
      Game.tick game


    Restart ->
      Game.makeInitialGame


    -- sets promoted piece into position on board
    Promote promotedPiecePosition figure ->
      Game.promotePiece game promotedPiecePosition figure


    -- Select represents a click on the board
    Click selectedPosition ->
      Game.handleClick game selectedPosition
