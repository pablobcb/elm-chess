-- TODO FIXME see if importing game i can get rid of the other imports
module View.Graveyard where

import String            exposing (..)
import Html              exposing (..)
import Html.Attributes   exposing (..)

import Chess.Game        exposing (..)
import Chess.Piece       exposing (..)
import Chess.Color       exposing (..)


renderGraveyardPiece : Color -> Figure -> Html
renderGraveyardPiece color figure  =
  let
    style = "graveyard square piece "
      ++ (toLower <| toString figure) ++ " "
      ++ (toLower <| toString color)  ++ " "

  in
    div [ class style ] []


renderGraveyard : Graveyard -> Color -> Html
renderGraveyard graveyard color =
  div [ class <| (++) "graveyard " <| toLower <| toString color]
    <| List.map (renderGraveyardPiece color) graveyard
