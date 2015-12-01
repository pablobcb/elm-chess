module View.Promotion where

import String                     exposing (..)
import Signal                     exposing (..)
import Html                       exposing (..)
import Html.Attributes            exposing (..)
import Html.Events                exposing (..)

import Update                     exposing (..)
import Chess.Game as Game         exposing (..)
import Chess.Piece                exposing (..)

import View.Piece                 exposing (..)

renderPromotion : Address Action -> Game -> Html
renderPromotion address game =
  case game.state of
    SelectPromotion position ->
      let
        handleClick piece =
         onClick address <| Promote position piece.figure

        getClass piece =
          String.join " "
            [ "square"
            , "promotion__piece"
            , getPieceClass piece
            ]

        renderPromotionButton piece =
          div
            [ class <| getClass piece , handleClick piece ]
            [ ]
      in
        div
          [ class "promotion active" ]
          [ div [ class "promotion__label" ] [ text "Promotion" ]
          , div
              [ class "promotion__pieces" ]
              [ renderPromotionButton <| piece Queen game.turn
              , renderPromotionButton <| piece Knight game.turn
              ]
          ]

    _ ->
      div [ class "promotion" ] [ ]
