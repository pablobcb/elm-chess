import StartApp.Simple as App

import Model  exposing (..)
import View   exposing (..)
import Update exposing (..)

--main : Signal.Html
main = App.start
    { model  = makeInitialGame
    , view   = renderGame
    , update = update
    }
