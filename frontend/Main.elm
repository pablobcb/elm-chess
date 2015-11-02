module Main(main) where

import Time        exposing (..)
import Html        exposing (..)

import Chess.Game  exposing (..)
import Chess.Color exposing (..)
import Chess.Board exposing (..)
import Chess.Piece exposing (..)
import View        exposing (..)
import Update      exposing (..)

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox UpdateTimer


actions : Signal Action
actions =
  inbox.signal


ticker : Signal Action
ticker =
  Signal.map (\_-> UpdateTimer) <| Time.every Time.second


model : Signal Game
model =
  Signal.foldp update makeInitialGame (Signal.merge actions ticker)


main : Signal Html
main = Signal.map (renderGame inbox.address) model
