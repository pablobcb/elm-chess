module Main(main) where

import StartApp.Simple as App
import Time   exposing (..)
import Html   exposing (..)

import Model  exposing (..)
import View   exposing (..)
import Update exposing (..)

inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox <| UpdateTimer 0


actions : Signal Action
actions =
  inbox.signal

--format timeInSeconds =

ticker : Signal Action
ticker =
  let
    update _ = (UpdateTimer 0)
  in
    Signal.map update (Time.every Time.second)


model : Signal Game
model =
  Signal.foldp update makeInitialGame (Signal.merge actions ticker)


main : Signal Html
main = Signal.map (renderGame inbox.address) model

--main = App.start
--    { model  = makeInitialGame
--    , view   = renderGame
--    , update = update
--    }
