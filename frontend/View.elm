module View where

import Debug exposing (..)

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

import Chess.Game      exposing (..)
import Chess.Color     exposing (..)
import Chess.Board     exposing (..)
import Chess.Piece     exposing (..)
import Update          exposing (..)

{-------------------------- Piece -------------------------}

getPieceClass : Piece -> String
getPieceClass piece =
  toLower <| String.join " " [ "piece"
                             , toString piece.figure
                             , toString piece.color
                             ]

{----------------------------- Board ----------------------------}

renderBoardSquare : Address Action -> GameState -> Position -> Square -> Html
renderBoardSquare address state position square =
  let
    highlight =
      (case state of
        Destination origin validPositions ->
          if List.member position validPositions
             || position == origin
          -- fix me: repetion of ""
          then " valid-destination"
          else ""

        _ -> "")
  in
    case square of
      Nothing ->
        div [ class <| "square" ++ highlight
            , onClick address (Select position)
            , title <| toString position
            ] []

      Just piece ->
        div [ class <| "square " ++ (getPieceClass piece) ++ highlight
            , onClick address (Select position)
            , title <| toString position
            ] []


renderBoard : Address Action -> Color -> GameState -> Board -> Html
renderBoard address turn state board =
  let
--    positions = keys board
    positions =
      List.concat <|
          List.map (\digit ->
            List.map (\letter->
               (letter, digit))
                 ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'])
                   [1 .. 8]

    pieces : List Square
    pieces =
      List.map (getSquareContent board) positions

    squares = List.map2 (renderBoardSquare address state) positions pieces

    highlight = case state of
      Origin ->
        String.join "-" ["highlight", toLower <| toString turn, "pieces"]


      _ ->
        ""

    className = String.join " " ["chessboard", highlight]

  in
    div [ class className ] squares


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
    prefix = "waiting for " ++ (toString game.turn)

    assert2digits str =
      if length str == 1
      then "0" ++ str
      else str

    time f = toString <| f game.turnInSeconds 60

    seconds = assert2digits <| time rem

    minutes = time (//)

    parsedTime = minutes ++ ":" ++ seconds

    statusBar =
      case game.state of
        Origin ->
          [ text <| prefix ++ " to select a piece" ]

        Destination _ _ ->
          [ text <| prefix ++ " to select a destination" ]

        Promotion position ->
          let
            queen = piece Queen game.turn

            knight = piece Knight game.turn

          in
            [ text "promote to:"
            , button
               [ onClick address <| Promote position queen.figure
               , class <| "square " ++ (getPieceClass queen)
               ] []
            , button
                [ onClick address <| Promote position knight.figure
                , class <| "square " ++ (getPieceClass knight)
                ] []
            ]

        Finished winner ->
          [ text
              <| "the game has ended, "
              ++ (toString winner)
              ++ " has won!"
          ]
  in
    div [ class "status-bar" ] (statusBar ++ [ text (" " ++ parsedTime )])


{----------------------------- Game ----------------------------}

renderGame : Address Action -> Game -> Html
renderGame address game =
  let
    breno = watch "turn" game.turn
    magro = watch "state" game.state
  in
    div [ class "game" ]
        [ renderStatusBar address game
        , div [ class "board-and-graveyard" ]
              [ renderGraveyard game.graveyard2 Black
              , renderBoard address game.turn game.state game.board
              , renderGraveyard game.graveyard1 White
              ]
        ]
