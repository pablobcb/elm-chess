module Chess.Game where

import Debug       exposing (..)

import Maybe       exposing (..)
import Maybe.Extra exposing (..)

import Dict        exposing (..)

import Chess.Color exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece exposing (..)

type alias Graveyard = List (Maybe Figure)


type alias Winner = Color


type GameState = Origin
           | Destination Position (List Position)
           | Promotion Position
           | EnPassant Position
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

remove : a -> List a -> List a
remove x = List.filter ((/=) x)

getValidDestinations : Position -> Piece -> Game -> List Position
getValidDestinations origin piece game =
  let
    getSquareContent' = getSquareContent game.board

    regularMoves  = watch "regular moves" <| getRegularMoves (ranges piece) origin

    allowedMoves =
      case piece.figure of
        Pawn ->
          let
            pawnTakeRanges' =
              pawnTakeRanges game.turn

            getSquareContent'' f =
              getSquareContent' <| Board.shift origin <| f pawnTakeRanges'

            takeToRight =
              case getSquareContent'' .right of
                Just piece'->
                  [ Board.shift origin (.right pawnTakeRanges') ]
                Nothing ->
                  []

            takeToLeft =
              case getSquareContent'' .left of
                 Just piece'->
                   [ Board.shift origin (.left pawnTakeRanges') ]
                 Nothing ->
                   []

            enPassant =
              case game.state of
                EnPassant passedPawnPosition ->
                  []
                _ -> []

            positionAhead =
              watch "positionAhead" <| Board.positionAhead game.turn origin

            isPositionAheadBlocked =
              watch "isPositionAheadBlocked" <| isJust <| getSquareContent game.board positionAhead

          in
            takeToLeft
            ++ takeToRight
            ++  enPassant
            ++ if isPositionAheadBlocked
               then watch "after remove" <| remove positionAhead regularMoves
               else regularMoves

        _ -> regularMoves

    -- a piece cant take an ally
    destinationHasNoAlly destination =
      case getSquareContent' destination of
        Just piece ->
          piece.color /= game.turn
        Nothing ->
          True

    filterDestinations destination =
      (destinationHasNoAlly destination)
      && origin /= destination

  in
    List.filter destinationHasNoAlly allowedMoves

