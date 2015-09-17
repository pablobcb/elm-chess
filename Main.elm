
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
import Maybe.Extra exposing (..)
import Array exposing  ( Array(..) )
import Text exposing   ( Text(..) )
import String exposing ( fromChar )
import Dict exposing (..)

import Model exposing (..)


--============================ Update ==============================
type alias Board = Dict String (Maybe Piece)
type alias Position = String
type Action = Click Position

--============================ View ================================
--mover tudo que retorna html para a view

getHtmlCode : Piece -> Html
getHtmlCode piece = text <| String.fromChar <| 
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


renderSquare : Maybe Piece -> Html
renderSquare piece = 
  td [] <| case piece of
    Just piece' -> [ getHtmlCode piece']
    Nothing -> []


renderRow : List (Maybe Piece) -> Html
renderRow pieces = 
  tr [] <| List.map (\piece ->
             case piece of
               Nothing -> td [] []
               _ -> renderSquare piece) pieces

getRow : Board -> List Position -> List (Maybe Piece)
getRow board positions = List.map
    (\key -> Maybe.Extra.join <| Dict.get key board)
    positions


renderBoard : Board -> Html
renderBoard board =
--  let getRow positions =
--    List.map (\key -> Dict.get key board) positions in
     table [ id "chessBoard" ] <| List.map (renderRow << getRow board)
           [ ["A1", "B1", "C1", "D1", "E1", "F1", "G1", "H1"]
           , ["A2", "B2", "C2", "D2", "E2", "F2", "G2", "H2"]
           , ["A3", "B3", "C3", "D3", "E3", "F3", "G3", "H3"]
           , ["A4", "B4", "C4", "D4", "E4", "F4", "G4", "H4"]
           , ["A5", "B5", "C5", "D5", "E5", "F5", "G5", "H5"]
           , ["A6", "B6", "C6", "D6", "E6", "F6", "G6", "H6"]
           , ["A7", "B7", "C7", "D7", "E7", "F7", "G7", "H7"]
           , ["A8", "B8", "C8", "D8", "E8", "F8", "G8", "H8"]
           ]

main : Html
main = renderBoard makeInitialBoard

