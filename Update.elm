module Update where 

import Model exposing (..)




type Action = Select Position
            | Promote Position
            | Restart

validatePosition a b  = True

move origin destination board = board

update : Action -> Game -> Game
update action game =
    case action of

      Restart ->
        makeInitialGame

      Promote selectedPosition ->
        game

      Select selectedPosition ->
        let
          squareContent = getSquareContent game.board selectedPosition

          player = game.turn

        in
          case game.state of

            Origin ->
              case squareContent of
                Nothing ->
                  game

                Just piece ->
                  { game 
                  | state <- Origin
                  , turn  <- other player
                  }


            Destination origin ->
              if validatePosition origin selectedPosition
              then
                let 
                  game' = { game | board <- move origin selectedPosition game.board }
                in 
                  if (snd selectedPosition == 1) || (snd selectedPosition == 8)
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

