module View where

import Debug exposing (..)

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Maybe           exposing ( Maybe(..) )

import Signal          exposing (..)
import String          exposing (..)

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

    highlight =
      case state of
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
clock : Game -> Html
clock game =
  let
    ensure2digits str =
      if length str == 1
      then "0" ++ str
      else str

    time f = toString <| f game.turnInSeconds 60

    seconds = ensure2digits <| time rem

    minutes = time (//)

    parsedTime = minutes ++ ":" ++ seconds

    turn = toLower <| toString game.turn

    clockClassName =
      "status-bar__clock status-bar__clock--" ++ turn

    clockIcon =
      span [ class "fa fa-clock-o status-bar__clock-icon" ] []

    clockMessage =
      let
        msg = "waiting for " ++ turn
      in
        span [ class "status-bar__clock-message" ] [ text msg ]
  in
   span [ class clockClassName ] [ clockIcon, text parsedTime, clockMessage ]

renderStatusBar : Address Action -> Game -> Html
renderStatusBar address game =
  let
    statusMsg =
      case game.state of
        Origin ->
          [ text "select a piece" ]

        Destination _ _ ->
          [ text  "to select a destination" ]

        Promotion position ->
          let
            queen = piece Queen game.turn

            knight = piece Knight game.turn

          in
            [ text "promote to:"
            , button
               [ onClick address <| Promote position queen.figure
               , class <| String.join " " [ getPieceClass queen
                                          , "square"
                                          , "status-bar__promotion-btn"
                                          ]
               ] []
            , button
                [ onClick address <| Promote position knight.figure
               , class <| String.join " " [ getPieceClass knight
                                          , "square"
                                          , "status-bar__promotion-btn"
                                          ]
                ] []
            ]

        Finished winner ->
          [ text
              <| "the game has ended, "
              ++ (toString winner)
              ++ " has won!"
          ]

    statusBar = [clock game] ++ statusMsg 

  in
    div [ class "status-bar" ] statusBar


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
