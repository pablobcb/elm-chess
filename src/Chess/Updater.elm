module Chess.Updater where

import Chess.Color as Color exposing (..)

resetClock game =
  { game | turnInSeconds = 0 }


tick game =
  { game
  | turnInSeconds = game.turnInSeconds + 1
  }


passTurn game =
  resetClock
    { game
    | previousState = game.state
    , turn = other game.turn
    }
