module Chess.SpecialMove where

import Dict exposing (..)

import Chess.Board as Board exposing (..)

type SpecialMove
  -- @Position:  the position behind the enemy pawn,
  --             where the attacking pawn will land
  --             after taking another pawn with EnPassant
  = EnPassant Position

  -- @Position:  the left or right position
  --             King will land after a Castling move
  | Castling ( Maybe Position, Maybe Position )

  -- @Position: position of the pawn which will be replaced for a new piece
  | Promotion Position


makeEnPassant : Board -> Position -> Position -> Position -> Board
makeEnPassant board origin destination enemyPawnPosition =
  let
    -- move piece behind enemy pawn
    (boardAfterPawnHasMoved, _) =
      Board.move board origin destination

    -- remove enemy pawn from game
    boardAfterEnPassant =
      Dict.insert
        enemyPawnPosition
        Nothing
        boardAfterPawnHasMoved

  in
    boardAfterEnPassant
