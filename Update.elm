module Update where

import Debug exposing (..)

import Model exposing (..)




type Action = Select Position
            | Promote Position Figure
            | Restart


update : Action -> Game -> Game
update action game =
    case action of

      Restart ->
        makeInitialGame

      -- sets promoted piece into position on board
      Promote promotedPiecePosition figure ->
        game

      -- Select represents a click on the board
      Select selectedPosition ->
        let
          -- piece or nothing
          squareContent = getSquareContent game.board selectedPosition

          player = game.turn

        in
          case game.state of
            -- ingores click on board because its waiting for a click from statusbar
            Promotion _ ->
              game

            -- sets origin as state and waits from a click
            -- on the board indicating the destination
            Origin ->
              case squareContent of
                Nothing ->
                  game

                Just piece ->
                  if game.turn /= piece.color
                  then game
                  else
                    { game
                    | state <- Destination selectedPosition
                    , turn  <- player
                    }

            -- validates the destination  nd checks if promotion
            -- should be granted to the moved piece
            Destination origin ->
              if validateMove origin selectedPosition game
              then -- valid move
                let
                  game' = move game origin selectedPosition

                  row = snd selectedPosition

                  isPawn =
                    let
                      squareContent = getSquareContent game'.board selectedPosition
                    in
                      case squareContent of
                        Just piece ->
                          if piece.figure == Pawn
                          then True
                          else False

                        Nothing ->
                          False

                  promoted = (row == 1 || row == 8) && isPawn

                in
                  if promoted
                  then -- sets state to promotion
                    { game'
                    | turn  <- player
                    , state <- Promotion selectedPosition
                    }
                  else -- passes the turn
                    { game'
                    | turn  <- other player
                    , state <- Origin
                    }
              else -- invalid move
                { game
                | turn  <- player
                , state <- Origin
                }

