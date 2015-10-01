module Update where

import Debug exposing (..)

import Dict exposing (..)

import Chess.Game  as Game exposing (..)
import Chess.Color as Game exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece as Game exposing (..)


-- tentar tirar tudo que seta o estado do jogo da update

type Action
  = UpdateTimer
  | Click Position
  | Promote Position Figure
  | Restart


update : Action -> Game -> Game
update action game =
  case action of

    UpdateTimer ->
      Game.tick game


    Restart ->
      Game.makeInitialGame


    -- sets promoted piece into position on board
    Promote promotedPiecePosition figure ->
      Game.promotePiece game promotedPiecePosition figure


    -- Select represents a click on the board
    Click selectedPosition ->
      case game.state of
        -- ingores click on board because its waiting for a click from statusbar
        Promotion _ ->
          game

        -- sets origin as state and waits from a click
        -- on the board indicating the destination
        Origin ->
          let
            selectedSquare =
              Board.getSquareContent game.board selectedPosition

          in
            case selectedSquare of
              Nothing -> -- ignores click because its expecting a piece
                game

              Just piece ->
                let
                  validDestinations =
                    Game.getValidDestinations
                      game
                      selectedPosition
                      piece
                in
                  if game.turn /= piece.color
                  then game
                  else
                    { game
                    | state <-
                        Destination
                          selectedPosition
                          validDestinations
                    }


        --  validates the destination
        -- checks if promotion happened
        -- checks if en passant happened
        Destination originPosition validDestinations ->
          if not <| List.member selectedPosition validDestinations
          then
            -- invalid move
            Game.waitForPieceSelection game
          else
          -- valid move
            let
              game' = Game.move game originPosition selectedPosition

              row = snd selectedPosition

              selectedDestination =
                getSquareContent game'.board selectedPosition

              isPawn =
                case selectedDestination of
                  Just piece ->
                    if piece.figure == Pawn
                    then True
                    else False

                  Nothing ->
                    False

              hasMovedTwoSquares =
                selectedPosition ==
                Board.shift originPosition
                  (case game'.turn of
                    White ->
                      (0, 2)

                    Black ->
                      (0, -2))

            in
              Game.passTurn <|
                if not isPawn
                then -- passes turn
                  Game.waitForPieceSelection game'
                else  -- checks pawn special states
                  if | row == 1 || row == 8 -> -- settng state to promotion
                         { game'
                         | state <- Promotion selectedPosition
                         }

                     | hasMovedTwoSquares -> -- setting state to enpassant
                         Game.waitForPieceSelection game'

                     | otherwise ->
                         Game.waitForPieceSelection game'
