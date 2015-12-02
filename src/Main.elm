module Main(main) where

import Time        exposing (..)
import Html        exposing (..)

import Update      exposing (..)

import Chess.Game  exposing (..)
import View.Game   exposing (..)

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox UpdateTimer


actions : Signal Action
actions =
  inbox.signal


ticker : Signal Action
ticker =
  Signal.map (always UpdateTimer) <| Time.every Time.second


model : Signal Game
model =
  Signal.foldp update makeInitialGame (Signal.merge actions ticker)


main : Signal Html
main = Signal.map (View.Game.renderGame inbox.address) model
