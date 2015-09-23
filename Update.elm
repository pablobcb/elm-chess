module Update where

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
                  { game
                  | state <- Origin
                  , turn  <- other player
                  }

            -- validates the destination  nd checks if promotion
            -- should be granted to the moved piece
            Destination origin ->
              if validatePosition origin selectedPosition
              then
                let
                  game' = { game | board <- move game.board origin selectedPosition }

                  row = snd selectedPosition

                  promoted = row == 1 || row == 8

                in
                  if promoted
                  then
                    { game'
                    | turn  <- player
                    , state <- Promotion selectedPosition
                    }
                  else
                    { game'
                    | turn  <- other player
                    , state <- Origin
                    }
              else
                { game
                | turn  <- player
                , state <- Origin
                }

