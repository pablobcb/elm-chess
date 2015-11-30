module Chess.Check where

import Maybe        exposing (..)
import List.Extra   exposing (..)
import Dict         exposing (..)

import Chess.Color        as Color        exposing (..)
import Chess.Board        as Board        exposing (..)
import Chess.Piece        as Piece        exposing (..)

-- FIXME TODO fazer arquivo de updaters

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


isKingInCheck : Color -> Board -> (Bool, Position)
isKingInCheck turn board =
  let
    squares : List (Position, Square)
    --squares =
    ---  List.map snd <| Dict.toList board
    squares =
      Dict.toList board

    --kingPosition : Position
    kingPosition =
      --List.Extra.find (squareSatisfiesPredicate <| isAlliedKing turn) squares
      (flip List.Extra.find) squares (\ (pos, square) ->
        case square of
          Nothing ->
            False

          Just piece ->
            piece.color == turn && piece.figure == King
      ) 

    --enemyPieces : List (Maybe Piece)
    --enemyPieces =
    --  List.filter (squareSatisfiesPredicate <| isEnemyPiece turn) squares

    ----enemyDestinations =
    --  List.filter
    --  (Board.getRegularDestinations turn board)

  in (,) True ('A', 8)


--getRegularDestinations : Color -> Board -> Piece -> Position -> List Position
--getRegularDestinations turn board piece position =
