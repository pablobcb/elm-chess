module Chess.Game where

import Maybe       exposing (..)
import Dict        exposing (..)

import Chess.Color exposing (..)
import Chess.Board exposing (..)
import Chess.Piece exposing (..)

type alias Graveyard = List (Maybe Figure)


type alias Winner = Color

type GameState = Origin
           | Destination Position
           | Promotion Position
           | CheckMate
           | Finished Winner

type alias Game =
  { board         : Board
  , graveyard1    : Graveyard
  , graveyard2    : Graveyard
  , turn          : Color
  , state         : GameState
  , turnInSeconds : Int
  }



makeInitialGame : Game
makeInitialGame =
  let
    emptyGraveyard = emptyRow ++ emptyRow
  in
    { board          = makeInitialBoard
    , graveyard1     = emptyGraveyard
    , graveyard2     = emptyGraveyard
    , turn           = White
    , state          = Origin
    , turnInSeconds  = 0
    }

move : Game -> Position -> Position -> Game
move game origin destination =
  let
    board = game.board

    destinationSquare =
      getSquareContent board destination

    originSquare =
      getSquareContent board origin

    board' = Dict.insert
      destination
      originSquare
      board

    game' =
      { game | board <- Dict.insert origin Nothing board'}

  in
    case destinationSquare of
      Just piece ->
        case game.turn of
          White ->
            { game'
            | graveyard2 <- game'.graveyard2 ++ [Just piece.figure]
            }

          Black ->
            { game'
            | graveyard1 <- game'.graveyard1 ++ [Just piece.figure]
            }

      Nothing ->
        game'
