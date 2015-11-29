module Chess.Check where

import Maybe        exposing (..)
import List.Extra   exposing (..)
import Dict         exposing (..)

import Chess.Color        as Color        exposing (..)
import Chess.Board        as Board        exposing (..)
import Chess.Piece        as Piece        exposing (..)


squareSatisfiesPredicate : (Piece -> Bool) -> Square -> Bool
squareSatisfiesPredicate predicate square =
  case square of
    Nothing ->
      False

    Just piece ->
      predicate piece

isAlliedKing : Color -> Piece -> Bool
isAlliedKing turn piece =
  piece.color == turn && piece.figure == King


isEnemyPiece : Color -> Piece -> Bool
isEnemyPiece turn piece = piece.color /= turn


isKingInCheck : Color -> Board -> Bool
isKingInCheck turn board =
  let
    squares =
      List.map snd <| Dict.toList board

    kingPosition =
      List.Extra.find (squareSatisfiesPredicate <| isAlliedKing turn) squares

    enemyPieces =
      List.filter (squareSatisfiesPredicate <| isEnemyPiece turn) squares


  in True
