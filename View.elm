module View where

import Dict            exposing (..)
import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Maybe           exposing ( Maybe(..) )
import Maybe.Extra exposing (..)

import Signal          exposing (..)
import StartApp.Simple exposing (..)
import String          exposing (..)
import Text            exposing ( Text(..) )

import Model exposing (..)
import Update exposing (..)

renderPiece : Piece -> Html
renderPiece piece =
  let
    className =
      case piece.figure of
        King -> case piece.color of
          Black -> "black king"
          White -> "white king"

        Queen -> case piece.color of
          Black -> "black queen"
          White -> "white queen"

        Rook -> case piece.color of
          Black -> "black rook"
          White -> "white rook"

        Bishop -> case piece.color of
          Black -> "black bishop"
          White -> "white bishop"

        Knight -> case piece.color of
          Black -> "black knight"
          White -> "white knight"

        Pawn -> case piece.color of
          Black -> "black pawn"
          White -> "white pawn"

  in div [ class <| "piece " ++ className ] [ ]

renderSquare : Address Action -> Position -> Maybe Piece -> Html
renderSquare address position piece =
  case piece of
    Nothing ->
      div [ class "square" ] [ ]

    Just piece' ->
      div [ class "square", onClick address (Origin position) ]
          [ renderPiece piece' ]

renderBoard : Address Action -> Board -> Html
renderBoard address board =
  let
    positions : List (Char, Int)
    positions =
      List.concat <|
        List.map (\digit ->
          List.map (\letter->
             (letter, digit))
               ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'])
                 [1 .. 8]

    pieces : List (Maybe Piece)
    pieces =
      List.map (getSquareContent board) positions

    squares = List.map2 (renderSquare address) positions pieces

  in div [ class "chessboard" ] squares


--renderGraveyard : Player -> Html
--renderGraveyard player =
--  let
--    renderChessboardSquare figure =
--      case figure of
--        Nothing ->
--          renderSquare

--        Just figure' ->
--          renderSquare <| piece figure' player.color
--  in div [ class <| (++) "graveyard " <| toLower <| toString player.color ]
--         ( List.map renderChessboardSquare player.graveyard )


renderStatusBar : String -> Html
renderStatusBar status =
  div [ class "status-bar" ] [ text status ]


renderGame : Address Action -> Game -> Html
renderGame address game =
  let p1 = game.player1
      p2 = game.player2

  in div [ class "game" ]
         [ div [ class "board-and-graveyard" ]
               [ --renderGraveyard p2
                renderBoard address game.board
               --, renderGraveyard p1
               ]
         , renderStatusBar "Lorem Ipsum"
         ]
