module View.StatusBar where

import Html            exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)
import String          exposing (..)

import Chess.Game  exposing (..)

import Update exposing (..)

formatTime : Int -> String
formatTime time =
  let
    ensure2digits str =
      if length str == 1
      then "0" ++ str
      else str

    time' f = toString <| f time 60
    seconds = ensure2digits <| time' rem
    minutes = time' (//)
  in
    minutes ++ ":" ++ seconds


clock : Game -> Html
clock game =
  let
    icon = span [ class "fa fa-clock-o status-bar__clock-icon" ] []
    time = text <| formatTime game.turnInSeconds
  in
   div [ class "status-bar__clock" ]
        [ icon
        , time
        ]


turnLabel : Game -> String
turnLabel game =
  game.turn |> toString |> toLower


turn : Game -> Html
turn game =
  let
    turn = (turnLabel game) ++ "s’ turn"
  in
    div [ class "status-bar__turn" ] [ text turn ]


renderStatusBar : Address Action -> Game -> Html
renderStatusBar address game =
  let
    statusMsg =
      case game.state of
        Origin _ ->
          [ text "select a piece" ]

        Destination _ _ _ ->
          [ text  "select a destination" ]

        SelectPromotion position ->
          [ text "select a piece" ]

        Finished winner ->
          [ text
              <| "The game has ended, "
              ++ (toString winner)
              ++ " has won!"
          ]

        _ -> []

    content =
      [ clock game
      , div [ class "status-bar__message"] statusMsg
      , turn game
      ]

    className =
      "status-bar status-bar--" ++ (turnLabel game)
  in
    div [ class className ] content
