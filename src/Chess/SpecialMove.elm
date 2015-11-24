module Chess.SpecialMove where

import Dict exposing (..)

import Chess.Board      as Board      exposing (..)
import Chess.Color      as Color      exposing (..)

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


noSpecialMove : ( List Position, Maybe SpecialMove )
noSpecialMove = ( [], Nothing )


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


makeCastling : Color -> Board -> Position -> Position -> Board
makeCastling color board origin destination =
  let
    -- move king to castling position
    ( boardAfterKingHasMoved, _ ) =
      Board.move board origin destination

    column = fst destination

    (rookOrigin, rookDestination) =
      if column == 'C'
      then
        ( Board.getLeftRookInitialPosition color
        , Board.positionLeft destination
        )
      else
        ( Board.getRightRookInitialPosition color
        , Board.positionRight destination
        )

    (boardAfterHookHasMoved, _) =
      Board.move boardAfterHookHasMoved rookOrigin rookDestination

  in
    boardAfterHookHasMoved
