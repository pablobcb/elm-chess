module Chess.Game where

import Maybe        exposing (..)
import Array        exposing (..)
import Maybe.Extra  exposing (..)
import Dict         exposing (..)

import Chess.Color        as Color        exposing (..)
import Chess.Board        as Board        exposing (..)
import Chess.SpecialMove  as SpecialMove  exposing (..)
import Chess.Piece        as Piece        exposing (..)
import Chess.Check        as Check        exposing (..)
import Chess.Updater      as Updater     exposing (..)

type alias Graveyard = List Figure


type GameState
  -- Waiting a player to select the piece he wants to move
  -- @(Maybe Position): indicates the position of a pawn if
  --                    it has moved 2 squares in the last play,
  --                    helping with enPassant detection
  =  Origin ( Maybe Position )

  -- Waiting a player to select a destination for the selected piece
  -- @Position:            the origin of the moving piece
  -- @(List Position):     list contaning a all valid destinations
  -- @(Maybe SpecialMove): indicates if and wich special move is possible
  | Destination Position ( List Position ) ( Maybe SpecialMove )

  -- Waiting a player to select the new piece for the promotion
  | SelectPromotion Position
  | CheckMate
  | Finished Color


type alias Game =
  { board         : Board
  , graveyard1    : Graveyard
  , graveyard2    : Graveyard
  , turn          : Color
  , state         : GameState
  , previousState : GameState
  , turnInSeconds : Int
  }


waitForPieceSelection : Maybe Position -> Game -> Game
waitForPieceSelection positionOfPawnWichMoved2Squares game =
  passTurn <|
    { game
    | state = Origin positionOfPawnWichMoved2Squares
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
      | board = board'
      , state = Origin Nothing
      }


makeInitialGame : Game
makeInitialGame =
  let
    emptyGraveyard = []
  in
    { board          = makeInitialBoard
    , graveyard1     = emptyGraveyard
    , graveyard2     = emptyGraveyard
    , turn           = White
    , state          = Origin Nothing
    , previousState  = Origin Nothing
    , turnInSeconds  = 0
    }


handleEnPassant : Game -> Position -> Position -> Game
handleEnPassant game originPosition selectedPosition =
    let
      enemyPawnPosition =
        Board.positionBelow
          game.turn
          selectedPosition

      board =
        SpecialMove.makeEnPassant
          game.board
          originPosition
          selectedPosition
          enemyPawnPosition

      gameAfterEnPassant =
        { game
        | board = board
        }
    in
      waitForPieceSelection Nothing gameAfterEnPassant


handleCastling : Game -> Position -> Position -> Maybe Position -> Maybe Position -> Game
handleCastling game originPosition selectedPosition kingLeftPosition kingRightPosition =
  let
    doCastling game origin destination =
      { game
      | board = SpecialMove.makeCastling
                  game.turn
                  game.board
                  origin
                  destination
      }

  in
    case kingLeftPosition of
      Just leftPosition ->
        if selectedPosition == leftPosition then
          doCastling game originPosition leftPosition
        else
          game

      Nothing ->
        case kingRightPosition of
          Just rightPosition ->
            if selectedPosition == rightPosition then
              doCastling game originPosition rightPosition
            else
              game

          Nothing ->
            Debug.crash """castling special move without both
              arguments being Nothing!"""


updateGraveyard : Game -> Figure -> Game
updateGraveyard game deadFigure =
  case game.turn of
    White ->
      { game
      | graveyard2 = game.graveyard2 ++ [deadFigure]
      }

    Black ->
      { game
      | graveyard1 = game.graveyard1 ++ [deadFigure]
      }


handleMove : Game -> Position -> Position -> Game
handleMove game origin destination =
  let
    (board, takenPiece) = Board.move game.board origin destination

    -- sends taken piece to graveyard
    game' =
      { game
      | board = board
      }

  in
    case takenPiece of
      Nothing ->
        game'

      Just piece ->
        updateGraveyard game' piece.figure


remove : a -> List a -> List a
remove x = List.filter ((/=) x)

getPawnValidTakes : Game -> Position -> List Position
getPawnValidTakes game origin =
  let
    -- checks if pawn cant take to right or left
    canTakeTo : ( { left : Range, right : Range } -> Range ) -> List Position
    canTakeTo f =
      let
        range : Range
        range =
          f <| Board.pawnTakeRanges game.turn

        enemyPiece : Maybe Piece
        enemyPiece =
          Board.getSquareContent game.board
            <| Board.shift origin range
      in
        if isJust enemyPiece
        then [ Board.shift origin range ]
        else []

    pawnTakePositions : List Position
    pawnTakePositions =
      ( canTakeTo .left ) ++ ( canTakeTo .right )

  in
      pawnTakePositions


noSpecialMove : ( List Position, Maybe SpecialMove )
noSpecialMove = ( [], Nothing )


getKingCastlingArrivalPosition : Game -> List Position -> ( Color -> Position ) -> Maybe Position
getKingCastlingArrivalPosition game castlingIntermediatePositions getRookInitialPosition =
  let
    rookPosition = getRookInitialPosition game.turn

    intermediatePositionsArray =
      Array.fromList castlingIntermediatePositions

    kingLandingPoint : Maybe Position
    kingLandingPoint = Array.get 1 intermediatePositionsArray

    rookSquare : Maybe Piece
    rookSquare = getSquareContent game.board rookPosition

  in
    Maybe.Extra.join
    <| (flip Maybe.map) rookSquare (\ piece ->
        if not (piece.figure == Rook && piece.moved == False) then
          Nothing
        else
          let
            isIntermediateCastlingSquaresEmpty : Bool
            isIntermediateCastlingSquaresEmpty =
              0 == (List.length <| List.filter
                  (isJust << getSquareContent game.board)
                  castlingIntermediatePositions)
          in
            if not isIntermediateCastlingSquaresEmpty then
              Nothing
            else
              kingLandingPoint)

getCastlingDestinations : Game -> Position -> Piece -> (List Position, Maybe SpecialMove)
getCastlingDestinations game origin king =
  if king.moved
  then
    noSpecialMove
  else
    let
      ( leftCastlingIntermediatePositions
      , rightCastlingIntermediatePositions
      ) =
        SpecialMove.getCastlingIntermediatePositions game.turn

      leftKingCastlingArrivalPosition =
        getKingCastlingArrivalPosition
          game
          leftCastlingIntermediatePositions
          SpecialMove.getLeftRookInitialPosition

      rightKingCastlingArrivalPosition =
        getKingCastlingArrivalPosition
          game
          rightCastlingIntermediatePositions
          SpecialMove.getLeftRookInitialPosition

      castlingPositions =
        (Maybe.Extra.maybeToList
          leftKingCastlingArrivalPosition)
        ++
        (Maybe.Extra.maybeToList
          rightKingCastlingArrivalPosition)

      noCastling =
        castlingPositions == []

    in
      if noCastling then
        noSpecialMove
      else
        ( castlingPositions
        , Just <| Castling
           ( leftKingCastlingArrivalPosition
           , rightKingCastlingArrivalPosition
           )
        )


getPawnSpecialDestinations : Game -> Position -> (List Position, Maybe SpecialMove)
getPawnSpecialDestinations game origin =
  let
    left = Board.positionLeft origin

    right = Board.positionRight origin

    nextPosition = Board.positionAhead game.turn origin

    nextRow = snd <| nextPosition

    ( specialDestinations, specialMove ) =
      if nextRow == 1 || nextRow == 8 then
        ([], Just <| Promotion nextPosition)
      else
        case game.state of
          Origin (Just pawnPosition) ->
            if pawnPosition == left || pawnPosition == right then
              let
                enPassantDestination : Position
                enPassantDestination =
                    Board.shift pawnPosition <|
                       case game.turn of
                         Black ->
                           (0, 1)

                         White ->
                           (0, -1)
              in
                ( [enPassantDestination]
                , Just <| EnPassant enPassantDestination
                )
            else
             noSpecialMove

          _ ->
             noSpecialMove

    destinations = (getPawnValidTakes game origin) ++ specialDestinations

  in
    (destinations, specialMove)


getSpecialDestinations : Game -> Position -> Piece ->
  ( List Position, Maybe SpecialMove )
getSpecialDestinations game origin piece =
  case piece.figure of
    Pawn ->
      getPawnSpecialDestinations
        game
        origin

    King ->
      getCastlingDestinations
        game
        origin
        piece

    _ ->
      noSpecialMove


getValidDestinations : Game -> Position -> Piece -> ( List Position, Maybe SpecialMove )
getValidDestinations game origin piece =
  let
    regularDestinations : List Position
    regularDestinations =
      Board.getRegularDestinations
        game.turn game.board piece origin

    ( specialDestinations, specialMove ) =
      getSpecialDestinations
        game origin piece

    -- a piece cant take an ally
    destinationHasNoAlly destination =
      case getSquareContent game.board destination of
        Just piece ->
          piece.color /= game.turn
        Nothing ->
          True

    filterDestinations destination =
      ( destinationHasNoAlly destination )
      && origin /= destination

    validDestinations =
      List.filter
        destinationHasNoAlly
        ( regularDestinations ++ specialDestinations )


  in
    ( validDestinations, specialMove )

--FIXME  rename
setValidDestinations : Game -> Position -> Game
setValidDestinations game selectedPosition =
  let

    selectedSquare =
      Board.getSquareContent game.board selectedPosition

  in
    case selectedSquare of

      Nothing -> -- ignores click because its expecting a piece
        game

      Just piece ->
        if game.turn /= piece.color
        then game -- ignores click because its expecting an enemy piece
        else
          let
            (validDestinations, specialMove) =
              getValidDestinations
                game
                selectedPosition
                piece

          in
            { game
            | state =
                  Destination
                  selectedPosition
                  validDestinations
                  specialMove
            }

-- FIXME rename
handleDestination :
  Game -> Position -> Position -> List Position -> Maybe SpecialMove -> Game
handleDestination game selectedPosition originPosition validDestinations specialMove =
  let

    isPositionValid = List.member selectedPosition validDestinations

    game' = handleMove game originPosition selectedPosition

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
          Black ->
            (0, 2)

          White ->
            (0, -2))

  in
    if not isPositionValid
    then
      -- invalid move
      { game
      | state = game.previousState
      }
    else
      case specialMove of
        Nothing ->
        -- valid move
            if not isPawn
            then -- passes turn
              waitForPieceSelection Nothing game'
            else
                 -- set the pawn position for enpassant detection
              if hasMovedTwoSquares then
                waitForPieceSelection (Just selectedPosition) game'
              else
                waitForPieceSelection Nothing game'


        Just (Promotion pos) ->
           { game'
           | previousState = game'.state
           , state = SelectPromotion pos
           }

        Just (EnPassant behindEnemyPawnPosition) ->
            if selectedPosition == behindEnemyPawnPosition
            then -- enpassant take detected
              handleEnPassant game originPosition selectedPosition
            else
              let
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

              in passTurn <|
                if not isPawn
                then -- passes turn
                  waitForPieceSelection Nothing game'
                else  -- checks pawn special states
                  if hasMovedTwoSquares then
                    waitForPieceSelection (Just selectedPosition) game'
                  else
                    waitForPieceSelection Nothing game'

        Just (Castling (kingLeftPosition, kingRightPosition)) ->
          waitForPieceSelection Nothing <|
            handleCastling
              game
              originPosition
              selectedPosition
              kingLeftPosition
              kingRightPosition



handleClick : Game -> Position -> Game
handleClick game selectedPosition =
  case game.state of
    -- ingores click on board because its waiting for a click from statusbar
    SelectPromotion _ ->
      game

    -- sets origin as state and waits from a click
    -- on the board indicating the destination
    Origin _ ->
      setValidDestinations game selectedPosition


    -- validates the destination
    -- checks if promotion or en passant occurred
    Destination originPosition validDestinations specialMove ->
      handleDestination game selectedPosition originPosition validDestinations specialMove

    CheckMate ->
      game

    Finished _->
      game
