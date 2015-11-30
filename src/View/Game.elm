module View.Game where

import Html            exposing (..)
import Html.Attributes exposing (..)
import Signal          exposing (..)

import Chess.Game      exposing (..)
import Chess.Color     exposing (..)

import View.StatusBar  exposing (..)
import View.Graveyard  exposing (..)
import View.Board      exposing (..)
import View.Promotion  exposing (..)

import Update          exposing (..)


renderGame : Address Action -> Game -> Html
renderGame address game =
    div [ class "game" ]
        [ div [ class "board-and-graveyard" ]
              [ renderGraveyard game.graveyard2 Black
              , renderBoard address game.turn game.state game.board
              , renderGraveyard game.graveyard1 White
              ]
        , renderStatusBar address game
        , renderPromotion address game
        ]
