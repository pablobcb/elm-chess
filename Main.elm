import StartApp.Simple

import Model  exposing (..)
import View   exposing (..)
import Update exposing (..)


main = StartApp.Simple.start
    { model  = makeInitialGame
    , view   = renderGame
    , update = update
    }
