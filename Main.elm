
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
--mover tudo que retorna html para a view

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


renderSquare : Square -> Html
renderSquare square = 
  td [ squareStyle square.color ] <| case square.piece of
    Just piece -> [ renderPiece piece]
    Nothing -> []


renderRow : List (Maybe Square) -> Html
renderRow squares = 
  tr [] <| List.map (\square ->
             case square of
               Just square' -> renderSquare square'
               Nothing -> td [] []) squares


getRow : Board -> List Position -> List (Maybe Square)
getRow board positions = List.map (\key -> Dict.get key board) positions


renderBoard : Board -> Html
renderBoard board =
  let getRow positions = List.map (\key -> Dict.get key board) positions
  in table [ id "chessBoard" ] <| List.map (renderRow << getRow)
    [ ["A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1"]
    , ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"]
    , ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"]
    , ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"]
    , ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"]
    , ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"]
    , ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"]
    , ["A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8"]
    ]
        
        

main = renderBoard makeInitialBoard