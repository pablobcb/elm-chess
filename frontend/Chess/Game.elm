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
  passTurn <|
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
        , color = game.turn
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


-- TODO: quebrar essa função em duas
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


handleClick : Game -> Position -> Game
handleClick game selectedPosition =
  case game.state of
    -- ingores click on board because its waiting for a click from statusbar
    Promotion _ ->
      game

    -- sets origin as state and waits from a click
    -- on the board indicating the destination
    Origin ->
      let
        selectedSquare =
          Board.getSquareContent game.board selectedPosition

      in
        case selectedSquare of
          Nothing -> -- ignores click because its expecting a piece
            game

          Just piece ->
            let
              validDestinations =
                getValidDestinations
                  game
                  selectedPosition
                  piece
            in
              if game.turn /= piece.color
              then game
              else
                { game
                | state <-
                    Destination
                      selectedPosition
                      validDestinations
                }

    EnPassant pawnPosition ->
      let
        selectedSquare =
          Board.getSquareContent game.board selectedPosition

      in
        case selectedSquare of
          Nothing -> -- ignores click because its expecting a piece
            game

          Just piece ->
            let
              validDestinations =
                getValidDestinations
                  game
                  selectedPosition
                  piece
            in
              if game.turn /= piece.color
              then game
              else
                { game
                | state <-
                    Destination
                      selectedPosition
                      validDestinations
                }

    --  validates the destination
    -- checks if promotion happened
    -- checks if en passant happened
    Destination originPosition validDestinations ->
      if not <| List.member selectedPosition validDestinations
      then
        -- invalid move
        { game
        | state <- Origin
        }
      else
      -- valid move
        let
          game' = move game originPosition selectedPosition

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

          hasMovedTwoSquares =
            selectedPosition ==
            Board.shift originPosition
              (case game'.turn of
                White ->
                  (0, 2)

                Black ->
                  (0, -2))

        in
          if not isPawn
          then -- passes turn
            waitForPieceSelection game'
          else  -- checks pawn special states
            if | row == 1 || row == 8 -> -- settng state to promotion
                   { game'
                   | state <- Promotion selectedPosition
                   }

               | hasMovedTwoSquares -> -- setting state to enpassant
                   { game'
                   | state <- EnPassant selectedPosition
                   , turn  <- other game'.turn
                   }

               | otherwise ->
                   waitForPieceSelection game'
