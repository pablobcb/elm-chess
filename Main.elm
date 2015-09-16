
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
import Maybe exposing  ( Maybe(..) )
import Array exposing  ( Array(..) )
import Text exposing   ( Text(..) )
import String exposing ( fromChar )
import Dict exposing (..)

import Model exposing (..)


--============================ Update ==============================
type alias Position = String
type Action = Click Position
--============================ View ================================


------------------------------ Style -------------------------------           

center = [ ("text-align", "center"), ("vertical-align", "middle") ]

boardStyle : Attribute
boardStyle =
    style <|
      [ 
      
      ] ++ center
--    style [ ("width" , "640px")
--          , ("height", "640px")
--          , ("border", "1x solid #000")
          -- ("margin", "1px")
--          ]


squareStyle : Color -> Attribute
squareStyle color =
  let bgColor = case color of
        Black -> ("background-color", "#808080")
        White -> ("background-color", "#0000")
  
  in style <| bgColor ::
    [ ("float", "left")
    , ("width", "80px")
    , ("height", "80px")
    --, ("border", "1px solid #000")
    , ("font-size", "400%")
    ] ++ center

------------------------- Render----------------------------------
renderPiece : Piece -> Html
renderPiece piece = text <| String.fromChar <| 
  case piece.figure of
    King -> case piece.color of
      Black -> '\x265A'
      White -> '\x2654'

    Queen -> case piece.color of
      Black -> '\x265B'
      White -> '\x2655'

    Rook -> case piece.color of
      Black -> '\x265C'
      White -> '\x2656'

    Bishop -> case piece.color of
      Black -> '\x265D'
      White -> '\x2657'

    Knight -> case piece.color of
      Black -> '\x265E'
      White -> '\x2658'

    Pawn -> case piece.color of
      Black -> '\x265F'
      White -> '\x2659'

getRow pos board = case Dict.get pos board of
    Just square -> Just <| renderSquare pos square
    Nothing -> Nothing

--mover tudo que retorna html para a view
renderRow : List Square -> Html
renderRow squares = 
  let row = List.map renderSquare squares
  in tr [] row

renderRows : List Square -> Html
renderRows rows =
  table [ id "chessBoard" ]

renderSquare : Position -> Square -> Html
renderSquare id' square = 
  td [ id id', squareStyle square.color ] <| case square.piece of
    Just piece -> [ renderPiece piece]
    Nothing -> []

--f id board =
-- case get id board of
-- Just 
-- List.map get ["A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8"] board
renderBoard board =
 -- let row1 = ["A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1"]
 --     row2 = ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"]
 --     row3 = ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"]
 --     row4 = ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"]
 --     row5 = ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"]
 --     row6 = ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"]
 --     row7 = ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"]
 --     row8 = ["A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8"]
  let squares = List.map Dict.get (keys board)

  in List.map

  table [ id "chessBoard" ]
        [ tr [] 
          --[ f "A8" makeInitialBoard
          [ renderSquare "A8" <| square White <| Just <| piece King   White False
          , renderSquare "B8" <| square White <| Just <| piece King   White False
          , renderSquare "C8" <| square Black <| Just <| piece Queen  Black False
          , renderSquare "D8" <| square White <| Just <| piece Queen  White False
          , renderSquare "E8" <| square Black <| Just <| piece Bishop Black False
          , renderSquare "F8" <| square White <| Just <| piece Bishop White False
          , renderSquare "G8" <| square Black <| Just <| piece Knight Black False
          , renderSquare "H8" <| square White <| Just <| piece Knight White False
          ]
        , tr []
          [ renderSquare "A7" <| square White <| Just <| piece Rook White False
          , renderSquare "B7" <| square Black <| Just <| piece Rook Black False
          , renderSquare "C7" <| square White <| Just <| piece Pawn Black False
          , renderSquare "D7" <| square Black <| Just <| piece Pawn White False
          , renderSquare "E7" <| square White Nothing
          , renderSquare "F7" <| square White Nothing
          , renderSquare "G7" <| square White Nothing
          , renderSquare "H7" <| square White Nothing
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
main = renderBoard
