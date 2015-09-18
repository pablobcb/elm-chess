
import Array           exposing ( Array(..) )
import Dict            exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Maybe           exposing ( Maybe(..) )
import Maybe.Extra     exposing (..)
import Signal          exposing (..)
import StartApp.Simple exposing (..)
import String          exposing (..)
import Text            exposing ( Text(..) )

import Model exposing (..)


--============================ Update ==============================

type Action = Click Position

update : Action -> Game -> Game
update action board =
    case action of
      Click position-> board


--============================ View ================================
--mover tudo que retorna html para a view

getHtmlCode : Piece -> Html
getHtmlCode piece = text
  <| String.fromChar
  <| case piece.figure of
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

renderEmptySquare : Html
renderEmptySquare = td [ style [("cursor", "default")] ] []


renderBoard : Address Action -> Board -> Html
renderBoard address board =
  -- combines two lists into their cartesian product
  let makePositions = 
        List.map (\digit ->
          List.map (\letter->
           (,) letter digit)
             ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'])
               ['1', '2', '3' , '4', '5', '6', '7', '8']


      renderPiece piece position =
        td [ style [("cursor", "grab")]
           , onClick address (Click position)
           ]
           [ getHtmlCode piece ]


      renderSquare position =
        let piece = Maybe.Extra.join <| Dict.get position board
        in case piece of
          Nothing ->
            renderEmptySquare
          
          Just piece' ->
            renderPiece piece' position


      renderRow positions =
        tr [] <| List.map renderSquare positions


  in table [ id "chessBoard" ]
       <| List.map renderRow
       <| makePositions

renderGraveyard : Player -> Html
renderGraveyard player =
  let renderPiece piece =
        td [] [ getHtmlCode piece ]


      renderSquare figure =
        case figure of
          Nothing ->
            renderEmptySquare

          Just figure' ->
            renderPiece <| piece figure' player.color


  in table [ id <| toString player.color ++ "Graveyard" ]
           <| List.map renderSquare player.graveyard

renderGame : Address Action -> Game -> Html
renderGame address game =
  let p1 = game.player1
      p2 = game.player2

  in div [ id "game" ]
         [ renderGraveyard p2
         , renderBoard address game.board
         , renderGraveyard p1
         ]

main = StartApp.Simple.start
    { model  = makeInitialGame
    , view   = renderGame
    , update = update
    }
