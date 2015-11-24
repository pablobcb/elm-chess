module View.StatusBar where

import Html            exposing (..)
import Html.Attributes exposing (..)
import Html.Events     exposing (..)
import Signal          exposing (..)
import String          exposing (..)

import Chess.Game  exposing (..)
import Chess.Piece exposing (..)

import Update exposing (..)

import View.Piece exposing (..)

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
        Origin _ ->
          [ text "select a piece" ]

        Destination _ _ _ ->
          [ text  "to select a destination" ]

        SelectPromotion position ->
          let
            renderPromotionBtn piece =
              button
                 [ onClick address <| Promote position piece.figure
                 , class <| String.join " " [ getPieceClass piece
                                            , "square"
                                            , "status-bar__promotion-btn"
                                            ]
                 ] []
          in
            [ text "promote to:"
            , renderPromotionBtn <| piece Queen game.turn
            , renderPromotionBtn <| piece Knight game.turn
            ]


        Finished winner ->
          [ text
              <| "the game has ended, "
              ++ (toString winner)
              ++ " has won!"
          ]

        _ -> []

    statusBar = [clock game] ++ statusMsg

  in
    div [ class "status-bar" ] statusBar
