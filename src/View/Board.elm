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


type GutterPlacement
  = Top
  | Bottom
  | Left
  | Right


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


renderGutterItem : String -> Html
renderGutterItem label =
  div [ class "chessboard__gutter__item" ] [ text label ]


renderGutterHorizontal : List Html
renderGutterHorizontal =
  List.map renderGutterItem [ "A", "B", "C", "D", "E", "F", "G", "H"]


renderGutterVertical : List Html
renderGutterVertical =
  List.map renderGutterItem [ "1", "2", "3", "4", "5", "6", "7", "8"]


renderGutter : GutterPlacement -> Html
renderGutter placement =
  case placement of
    Top ->
      div [ class "chessboard__gutter top"] renderGutterHorizontal

    Left ->
      div [ class "chessboard__gutter left"] renderGutterVertical

    Right ->
      div [ class "chessboard__gutter right" ] renderGutterVertical

    Bottom ->
      div [ class "chessboard__gutter bottom "] renderGutterHorizontal


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
    div [ class className ] [ renderGutter Left
                            , renderGutter Top
                            , div [ class "chessboard__grid" ] squares
                            , renderGutter Right
                            , renderGutter Bottom
                            ]
