module Update where 

import Model exposing (..)

type Action = Origin Position
            | Destination Position
            | Promotion Figure
            | Restart


update : Action -> Game -> Game
update action game =
    case action of
      Origin position ->
        { game | board <- game.board }

      Restart ->
        makeInitialGame
