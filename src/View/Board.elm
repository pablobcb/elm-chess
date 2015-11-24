module View.Board where

import String            exposing (..)
import Signal            exposing (..)
import Html              exposing (..)
import Html.Attributes   exposing (..)
import Html.Events       exposing (..)

import Update            exposing (..)

import Chess.Game  as Game  exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Color as Color exposing (..)

import View.Piece        exposing (..)


renderBoardSquare : Address Action -> GameState -> Position -> Square -> Html
renderBoardSquare address state position square =
  let
    highlight =
      (case state of
        Destination origin validPositions _ ->
          if List.member position validPositions
             || position == origin
          then "valid-destination"
          else ""

        _ -> "")

    title' =
      title <| toString position

    emitPosition =
      onClick address <| Click position

    renderSquare pieceStyle =
      div [ class <| String.join " " [ "square", highlight, pieceStyle ]
          , emitPosition
          , title'
          ] []

 in
    case square of
      Nothing ->
        renderSquare ""

      Just piece ->
        renderSquare <| getPieceClass piece


renderBoard : Address Action -> Color -> GameState -> Board -> Html
renderBoard address turn state board =
  let
    positions = Board.getPositions

    pieces : List Square
    pieces =
      List.map (getSquareContent board) positions

    squares = List.map2 (renderBoardSquare address state) positions pieces

    highlight =
      case state of
        Origin _ ->
          String.join "-" ["highlight", toLower <| toString turn, "pieces"]

        _ ->
          ""

    className = String.join " " ["chessboard", highlight]

  in
    div [ class className ] squares
