module Update where

import Debug exposing (..)

import Model exposing (..)
import Dict exposing (..)




type Action = Select Position
            | Promote Position Figure
            | Restart


update : Action -> Game -> Game
update action game =
  let
    player : Color
    player = game.turn

    board = game.board
  in
    case action of

      Restart ->
        makeInitialGame


      -- sets promoted piece into position on board
      Promote promotedPiecePosition figure ->
        let
          promotedTo = Just <| { figure = figure, moved = True, color = player }

          board' = Dict.insert promotedPiecePosition promotedTo board
        in
          { game
          | board <- board'
          , turn  <- other player
          , state <- Origin
          }


      -- Select represents a click on the board
      Select selectedPosition ->
        case game.state of
          -- ingores click on board because its waiting for a click from statusbar
          Promotion _ ->
            game


          -- sets origin as state and waits from a click
          -- on the board indicating the destination
          Origin ->
            let
              selectedOrigin = getSquareContent board selectedPosition
            in
              case selectedOrigin of
                Nothing ->
                  game


                Just piece ->
                  if player /= piece.color
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

