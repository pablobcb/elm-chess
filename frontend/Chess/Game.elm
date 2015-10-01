module Chess.Game where

import Debug       exposing (..)

import Maybe       exposing (..)
import Maybe.Extra exposing (..)

import Dict        exposing (..)

import Chess.Color exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece exposing (..)

type alias Graveyard = List (Maybe Figure)


type GameState
  = Origin
  | Destination Position (List Position)
  | Promotion Position
  | EnPassant Position
  | CheckMate
  | Finished Color


type alias Game =
  { board         : Board
  , graveyard1    : Graveyard
  , graveyard2    : Graveyard
  , turn          : Color
  , state         : GameState
  , turnInSeconds : Int
  }


resetClock : Game -> Game
resetClock game =
  { game
  | turnInSeconds <- 0
  }


tick : Game -> Game
tick game =
  { game
  | turnInSeconds <- game.turnInSeconds + 1
  }


passTurn : Game -> Game
passTurn game =
  resetClock
    { game
    | turn <- other game.turn
    }


waitForPieceSelection : Game -> Game
waitForPieceSelection game =
  { game
  | state <- Origin
  }


promotePiece : Game -> Position -> Figure -> Game
promotePiece game promotedPiecePosition figure =
  let
    promotedTo =
      Just <|
        { figure = figure
        , moved = True
        , color = other game.turn
        }

    board' =
      Dict.insert
        promotedPiecePosition
        promotedTo
        game.board
  in
    passTurn <|
      { game
      | board <- board'
      , state <- Origin
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
      Board.getSquareContent board destination

    originSquare =
      Board.getSquareContent board origin

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
      { game
      | board <- Dict.insert origin Nothing board'
      }

    updateGraveyard graveyard figure =
      List.drop 1 <| graveyard ++ [ Just figure ]

  in
    case destinationSquare of
      Nothing -> --just move
        game'

      Just piece -> --take piece moving it to the correct graveyard
        case game'.turn of
          White ->
            { game'
            | graveyard2 <- updateGraveyard game'.graveyard2 piece.figure
            }

          Black ->
            { game'
            | graveyard1 <- updateGraveyard game'.graveyard1 piece.figure
            }


remove : a -> List a -> List a
remove x = List.filter ((/=) x)


getValidDestinations : Game -> Position -> Piece -> List Position
getValidDestinations game origin piece =
  let
    getSquareContent' = getSquareContent game.board

    regularMoves  = getRegularMoves game.turn game.board piece origin

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
              Board.positionAhead game.turn origin

            isPositionAheadBlocked =
              isJust <| getSquareContent game.board positionAhead

          in
            takeToLeft
            ++ takeToRight
            ++  enPassant
            ++ if isPositionAheadBlocked
               then remove positionAhead regularMoves
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

