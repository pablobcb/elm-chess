module Chess.Game where

import Maybe       exposing (..)
import Dict        exposing (..)

import Chess.Color exposing (..)
import Chess.Board as Board exposing (..)
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

    board' =
      let
        piece =
          case originSquare of
            Just piece' -> Just
              { piece'
              | moved <- True
              }
      in -- copies piece to destination
        Dict.insert destination piece board

    game' = --cleans origin
      { game | board <- Dict.insert origin Nothing board'}

  in
    case destinationSquare of
      Just piece -> --take piece
        case game.turn of
          White ->
            { game'
            | graveyard2 <- game'.graveyard2 ++ [Just piece.figure]
            }

          Black ->
            { game'
            | graveyard1 <- game'.graveyard1 ++ [Just piece.figure]
            }

      Nothing -> --just move
        game'

validateMove : Position -> Position -> Game -> Bool
validateMove origin destination game =
  let
    board =  game.board

    getSquareContent' = getSquareContent board

    originSquare : Square
    originSquare = getSquareContent' origin


    destinationSquare : Square
    destinationSquare = getSquareContent' destination


    isDestinationValid : Bool
    isDestinationValid =
      case originSquare of
        Just piece ->
          let
            validPositions : List Position
            validPositions = getValidPositions(ranges piece) origin

            specialPositions : List Position
            specialPositions =
              case piece.figure of
                Pawn ->
                  let
                    pawnTakeRanges' = pawnTakeRanges game.turn

                    getSquareContent'' f =
                      getSquareContent' <| Board.shift origin <| f pawnTakeRanges'


                    right =
                      case getSquareContent'' .right of
                        Just piece'->
                          [ Board.shift origin (.right pawnTakeRanges') ]
                        Nothing ->
                          []

                    left =
                      case getSquareContent'' .left of
                         Just piece'->
                           [ Board.shift origin (.left pawnTakeRanges') ]
                         Nothing ->
                           []
                  in
                    (++) right left


                _ -> []

          in
            List.member destination (validPositions ++ specialPositions )


    otherColor : Bool
    otherColor =
      case destinationSquare of
        Just piece ->
          (piece.color /= game.turn)

        Nothing ->
          True

  in
     -- a piece cant move to the same place
     (origin /= destination)
     -- a piece cant move to a place out of range
     && isDestinationValid
     -- a piece cant take an ally
     && otherColor
