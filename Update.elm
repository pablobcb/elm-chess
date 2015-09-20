module Update where 

import Model exposing (..)

type Action = Origin Position
            | Destination Position
            | Promotion Figure
            | Restart


update : Action -> Game -> Game
update action board =
    case action of
      Origin position ->
        board
