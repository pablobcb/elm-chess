module Chess.Game where

import Debug       exposing (..)

import Maybe       exposing (..)
import Maybe.Extra exposing (..)

import Dict        exposing (..)

import Chess.Color exposing (..)
import Chess.Board as Board exposing (..)
import Chess.Piece exposing (..)

type alias Graveyard = List (Maybe Figure)

type SpecialMove
  -- @Position:  the position behind the enemy pawn,
  --             where the attacking pawn will land
  --             after taking another pawn with EnPassant
  = EnPassant Position

  -- @Position:  the left or right position
  --             King will land after a Castling move
  | Castling ( Maybe Position, Maybe Position )

  -- @Position: position of the pawn which will be replaced for a new piece
  | Promotion Position


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


resetClock : Game -> Game
resetClock game =
  { game | turnInSeconds = 0 }


tick : Game -> Game
tick game =
  { game
  | turnInSeconds = game.turnInSeconds + 1
  }


passTurn : Game -> Game
passTurn game =
  resetClock
    { game
    | previousState = game.state
    , turn = other game.turn
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
    emptyGraveyard = emptyRow ++ emptyRow
  in
    { board          = makeInitialBoard
    , graveyard1     = emptyGraveyard
    , graveyard2     = emptyGraveyard
    , turn           = White
    , state          = Origin Nothing
    , previousState  = Origin Nothing
    , turnInSeconds  = 0
    }


makeEnPassant : Game -> Position -> Position -> Position -> Game
makeEnPassant game origin destination enemyPawnPosition =
  let
    -- move piece behind enemy pawn
    gameMovedPawn = move game origin destination

    -- remove enemy pawn from game
    board = Dict.insert enemyPawnPosition Nothing gameMovedPawn.board

    -- put enemy pawn in the graveyard
    gameAfterEnPassant = updateGraveyard gameMovedPawn Pawn

  in
    { gameAfterEnPassant
    | board = board
    }

makeCastling : Game -> Position -> Position -> Game
makeCastling game origin destination =
  let
    -- move king to castling position
    gameMovedKing = move game origin destination

    column = fst destination

    -- TODO FIXME Duplicated code
    (rookOrigin, rookDestination) =
      if column == 'C'
      then ( Board.getLeftRookInitialPosition game.turn,  Board.positionLeft destination  )
      else ( Board.getRightRookInitialPosition game.turn, Board.positionRight destination )

    gameMovedRook = move gameMovedKing rookOrigin rookDestination

  in
    gameMovedRook


updateGraveyard : Game -> Figure -> Game
updateGraveyard game deadFigure =
  let
    updateGraveyard graveyard  =
      List.drop 1 <| graveyard ++ [ Just deadFigure ]
  in
    case game.turn of
      White ->
        { game
        | graveyard2 = updateGraveyard game.graveyard2
        }

      Black ->
        { game
        | graveyard1 = updateGraveyard game.graveyard1
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
          Maybe.map (\ piece -> { piece | moved = True }) originSquare

      in -- copies piece to destination
        Dict.insert
          destination
          piece
          board

    game' = --cleans origin
      { game
      | board = Dict.insert origin Nothing board'
      }

  in
    case destinationSquare of
      Nothing -> --just move
        game'

      Just piece -> --take piece moving it to the correct graveyard
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


-- remove nasty duplicated code
getKingCastlingArrivalPositionLeft : Game -> { fst : Position, snd : Position, thrd: Position } -> Maybe Position
getKingCastlingArrivalPositionLeft game castlingIntermediatePositions =
  let
    getSquareContent' = Board.getSquareContent game.board

    leftRookPosition = Board.getRightRookInitialPosition game.turn

    rightRookPosition = Board.getLeftRookInitialPosition game.turn

    kingLandingPoint : Position
    kingLandingPoint = .snd castlingIntermediatePositions

    leftRookSquare : Maybe Piece
    leftRookSquare = getSquareContent' leftRookPosition

  in
    Maybe.Extra.join
    <| (flip Maybe.map) leftRookSquare (\ piece ->
        if not (piece.figure == Rook && piece.moved == False) then
          Nothing
        else
          let
            isIntermediateCastlingSquaresEmpty : Bool
            isIntermediateCastlingSquaresEmpty =
              (isNothing <| getSquareContent' <| .fst castlingIntermediatePositions) &&
              (isNothing <| getSquareContent' <| .snd castlingIntermediatePositions) &&
              (isNothing <| getSquareContent' <| .thrd castlingIntermediatePositions)
          in
            if not isIntermediateCastlingSquaresEmpty then
              Nothing
            else
              Just <| kingLandingPoint)


getKingCastlingArrivalPositionRight : Game -> { fst : Position, snd : Position } -> Maybe Position
getKingCastlingArrivalPositionRight game castlingIntermediatePositions =
  let
    getSquareContent' = Board.getSquareContent game.board

    leftRookPosition = Board.getRightRookInitialPosition game.turn

    rightRookPosition = Board.getLeftRookInitialPosition game.turn

    kingLandingPoint : Position
    kingLandingPoint = .snd castlingIntermediatePositions

    leftRookSquare : Maybe Piece
    leftRookSquare = getSquareContent' leftRookPosition

  in
    Maybe.Extra.join
    <| (flip Maybe.map) leftRookSquare (\ piece ->
        if not (piece.figure == Rook && piece.moved == False) then
          Nothing
        else
          let
            isIntermediateCastlingSquaresEmpty : Bool
            isIntermediateCastlingSquaresEmpty =
              (isNothing <| getSquareContent' <| .fst castlingIntermediatePositions) &&
              (isNothing <| getSquareContent' <| .snd castlingIntermediatePositions)
          in


            if not isIntermediateCastlingSquaresEmpty then
              Nothing
            else
              Just <| kingLandingPoint)



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
        Board.getCastlingIntermediatePositions game.turn

      leftKingCastlingArrivalPosition =
        getKingCastlingArrivalPositionLeft game leftCastlingIntermediatePositions

      rightKingCastlingArrivalPosition =
        getKingCastlingArrivalPositionRight game rightCastlingIntermediatePositions

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


getSpecialDestinations : Game -> Position -> Piece ->
  ( List Position, Maybe SpecialMove )
getSpecialDestinations game origin piece =
  case piece.figure of
    Pawn ->
      getPawnSpecialDestinations game origin

    King ->
      getCastlingDestinations game origin piece

    _ ->
      noSpecialMove

getValidDestinations : Game -> Position -> Piece ->
  ( List Position, Maybe SpecialMove )
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

    game' = move game originPosition selectedPosition

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
              waitForPieceSelection Nothing <|
                makeEnPassant game originPosition selectedPosition <|
                  Board.positionBelow game.turn selectedPosition
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
            case kingLeftPosition of
              Just leftPosition ->
                if selectedPosition == leftPosition then
                  makeCastling game originPosition leftPosition
                else
                  game'
              Nothing ->
                case kingRightPosition of
                  Just rightPosition ->
                    if selectedPosition == rightPosition then
                      makeCastling game originPosition rightPosition
                    else
                      game'

                  Nothing ->
                    Debug.crash "castling special move without both arguments being Nothing!"


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
