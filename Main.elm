
import Array exposing  ( Array(..) )
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing  ( Maybe(..) )
import Maybe.Extra exposing (..)
import Signal exposing (..)
import StartApp.Simple exposing (..)
import String exposing (..)
import Text exposing   ( Text(..) )

import Model exposing (..)


--============================ Update ==============================

type Action = Click Position

update : Action -> Board -> Board
update action board =
    case action of
      Click position-> board


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


renderBoard : Address Action -> Board -> Html
renderBoard address board =

  let letters = ["A", "B", "C", "D", "E", "F", "G", "H"]
      numbers = List.map toString [1 .. 8]
      renderSquare position = 
        let piece = Maybe.Extra.join <| Dict.get position board
        in case piece of
          Nothing     -> td [ style [("cursor", "default")] ] []
          Just piece' -> td [ style [("cursor", "grab")]
                            , onClick address (Click position)
                            , id position
                            ]
                            [ getHtmlCode piece' ]
  
  in
     table [ id "chessBoard" ] 
          <| List.map (\squares-> tr []
          <| List.map renderSquare squares)
          <| List.map (\letter -> 
             List.map (\num-> String.append num letter) letters) numbers

main = StartApp.Simple.start
    { model  = makeInitialBoard
    , view   = renderBoard
    , update = update
    }
