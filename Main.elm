
--import Counter exposing (update, view)
--import StartApp.Simple exposing (start)


--main =
  --start
--    { model = 0
--    , update = update
--    , view = view
--    }

import Html exposing (..)
import Html.Attributes exposing (..)

boardStyle : Attribute
boardStyle =
  style [ ("width" , "640px")
        , ("height", "640px")
        , ("margin", "0.5em")
        , ("border", "2px solid #808080")
        ]
        
main =
  table [ id "chessBoard", colspan 8, rowspan 8, boardStyle ]
        [ tr [] 
          [ td [ id "A8" ] []
          , td [ id "B8" ] []
          , td [ id "C8" ] []
          , td [ id "D8" ] []
          , td [ id "E8" ] []
          , td [ id "F8" ] []
          , td [ id "G8" ] []
          , td [ id "H8" ] []
          ]
        , tr []
          [ td [ id "A7" ] []
          , td [ id "B7" ] []
          , td [ id "C7" ] []
          , td [ id "D7" ] []
          , td [ id "E7" ] []
          , td [ id "F7" ] []
          , td [ id "G7" ] []
          , td [ id "H7" ] []
          ]
        , tr []
          [ td [ id "A6" ] []
          , td [ id "B6" ] []
          , td [ id "C6" ] []
          , td [ id "D6" ] []
          , td [ id "E6" ] []
          , td [ id "F6" ] []
          , td [ id "G6" ] []
          , td [ id "H6" ] []
          ]
        , tr []
          [ td [ id "A5" ] []
          , td [ id "B5" ] []
          , td [ id "C5" ] []
          , td [ id "D5" ] []
          , td [ id "E5" ] []
          , td [ id "F5" ] []
          , td [ id "G5" ] []
          , td [ id "H5" ] []
          ]
        , tr []
          [ td [ id "A4" ] []
          , td [ id "B4" ] []
          , td [ id "C4" ] []
          , td [ id "D4" ] []
          , td [ id "E4" ] []
          , td [ id "F4" ] []
          , td [ id "G4" ] []
          , td [ id "H4" ] []
          ]
        , tr []
          [ td [ id "A3" ] []
          , td [ id "B3" ] []
          , td [ id "C3" ] []
          , td [ id "D3" ] []
          , td [ id "E3" ] []
          , td [ id "F3" ] []
          , td [ id "G3" ] []
          , td [ id "H3" ] []
          ]
        , tr []
          [ td [ id "A2" ] []
          , td [ id "B2" ] []
          , td [ id "C2" ] []
          , td [ id "D2" ] []
          , td [ id "E2" ] []
          , td [ id "F2" ] []
          , td [ id "G2" ] []
          , td [ id "H2" ] []
          ]
        , tr []
          [ td [ id "A1" ] []
          , td [ id "B1" ] []
          , td [ id "C1" ] []
          , td [ id "D1" ] []
          , td [ id "E1" ] []
          , td [ id "F1" ] []
          , td [ id "G1" ] []
          , td [ id "H1" ] []
          ]
        ]
