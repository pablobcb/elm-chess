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

{-------------------------- Piece -------------------------}

getPieceClass : Piece -> String
getPieceClass piece =
  let className =
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

  in "piece " ++ className


{----------------------------- Board ----------------------------}

renderBoardSquare : Address Action -> Position -> Square -> Html
renderBoardSquare address position square =
  case square of
    Nothing ->
      div [ class "square"
          , onClick address (Select position)
          ] []

    Just piece ->
      div [ class <| "square " ++ (getPieceClass piece)
          , onClick address (Select position)
          ] []


renderBoard : Address Action -> Board -> Html
renderBoard address board =
  let
    positions = keys board

    pieces : List Square
    pieces =
      List.map (getSquareContent board) positions

    squares = List.map2 (renderBoardSquare address) positions pieces

  in
    div [ class "chessboard" ] squares


{----------------------------- Graveyard ----------------------------}

renderGraveyardSquare : Square -> Html
renderGraveyardSquare square =
  let
    attrs =
      case square of
        Nothing -> ""
        Just piece -> getPieceClass piece

  in
    div [ class ("graveyard square " ++ attrs) ] []



renderGraveyard : Graveyard -> Color -> Html
renderGraveyard graveyard color =
  let
    renderSquare figure =
      case figure of
        Nothing ->
          renderGraveyardSquare Nothing
        Just figure' ->
          renderGraveyardSquare <| Just <| piece figure' color
  in
    div [ class <| (++) "graveyard " <| toLower <| toString color ]
        ( List.map renderSquare graveyard )


{----------------------------- Status Bar ----------------------------}

renderStatusBar : Address Action -> Game -> Html
renderStatusBar address game =
  let
    prefix = "waiting for " ++ (toString game.turn) ++ " player"

    status =
      case game.state of
        Origin ->
           prefix ++ " to select a piece"

        Destination _ ->
          prefix ++ " to select a destination"

        Promotion _ ->
          "Promotion"

        Finished winner ->
          "the game has ended, " ++
          (toString winner) ++ " has won!"

  in
    div [ class "status-bar" ] [ text status ]


{----------------------------- Game ----------------------------}

renderGame : Address Action -> Game -> Html
renderGame address game =
  div [ class "game" ]
      [ renderStatusBar address game
      , div [ class "board-and-graveyard" ]
            [ renderGraveyard game.graveyard2 White
            , renderBoard address game.board
            , renderGraveyard game.graveyard1 Black
            ]
      ]


