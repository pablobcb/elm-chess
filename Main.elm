
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
renderEmptySquare = td [ class "cursorDefault"] []


renderBoard : Address Action -> Board -> Html
renderBoard address board =
  -- combines two lists into their cartesian product
  let makeRows = 
        List.map (\digit ->
          List.map (\letter->
           (,) letter digit)
             ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'])
               [1, 2, 3 , 4, 5, 6, 7, 8]


      renderPiece piece position =
        td [ class "selectNone cursorGrab"
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
       <| makeRows

renderGraveyard : Player -> Html
renderGraveyard player =
  let renderPiece piece =
        td [ class "grave selectNone" ] [ getHtmlCode piece ]


      renderSquare figure =
        case figure of
          Nothing ->
            renderEmptySquare

          Just figure' ->
            renderPiece <| piece figure' player.color


      renderRow row =
        tr [] <| List.map renderSquare row


  in table [ class <| (++) "graveyard"  <| toString player.color ]
           [ renderRow <| List.take 8 player.graveyard
           , renderRow <| List.drop 8 player.graveyard
           ]


renderGame : Address Action -> Game -> Html
renderGame address game =
  let p1 = game.player1
      p2 = game.player2

  in div [ id "game" ]
         [ renderGraveyard p2
         , renderBoard address game.board
         , renderGraveyard p1
         , renderStatusBar "StatusBarText"
         ]


renderStatusBar : String -> Html
---renderStatusBar : Status -> Html
renderStatusBar status = 
 -- case status of
   -- Waiting ->

  div [ id "statusBar" ] [ text status ]


main = StartApp.Simple.start
    { model  = makeInitialGame
    , view   = renderGame
    , update = update
    }
