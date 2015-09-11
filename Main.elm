
import Counter exposing (update, view)
import StartApp.Simple exposing (start)


--main =
  --start
--    { model = 0
--    , update = update
--    , view = view
--    }

import Html exposing (..)
import Html.Attributes exposing (..)

main =
  table [ id "chessBoard", colspan 8, rowspan 8 ]
        [ tr [] 
          [ td [ id "A8" ] [text "BRENO"]
          , td [ id "A7" ] [text "BRENO"]  
          ]
        , tr []
          [ td [ id "A8" ] [text "BRENO"]
          , td [ id "A7" ] [text "BRENO"]  
          ]
        ]
