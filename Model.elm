module Model where

import Maybe       exposing (..)
import Maybe.Extra exposing (..)
import Dict        exposing (..)
import List        exposing (..)

{-------------------------- Color ---------------------------}
type Color = Black
           | White


other : Color -> Color
other color =
  case color of
    Black -> White
    White -> Black


{-------------------------- Piece ---------------------------}
type Figure = Pawn
            | Knight
            | Bishop
            | Rook
            | Queen
            | King


type alias Piece =
  { figure : Figure
  , moved  : Bool
  , color  : Color
  }


piece : Figure -> Color -> Piece
piece f c =
  { figure = f
  , color  = c
  , moved  = False
  }


{-------------------------- Board ---------------------------}
type alias Position = (Char, Int)

type alias Square = Maybe Piece

type alias Board = Dict Position Square

getSquareContent : Board -> Position -> Square
getSquareContent board =
  Maybe.Extra.join << (flip Dict.get) board


validateMove origin destination game =
  let
    otherColor =
      case getSquareContent game.board destination of
        Just piece ->
          piece.color /= game.turn

        Nothing ->
          True

  in
    (origin /= destination) -- a piece cant move to the same place
     && otherColor          -- a piece cant take an ally

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


emptyRow : List (Maybe a)
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = repeat 8
        <| Just
        <| piece Pawn pawnColor


      makePiece pieceColor figure =
          Just <| piece figure pieceColor


      makeFirstRow color = List.map
        (makePiece color)
        [ Rook, Knight, Bishop , Queen
        , King, Bishop, Knight, Rook
        ]


      zip = List.map2 (,)


      makeRow number = zip
        ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
        (List.repeat 8 number)


  in Dict.fromList <| zip (makeRow 8) (makeFirstRow Black)
                   ++ zip (makeRow 7) (pawnRow Black)
                   ++ zip (makeRow 6) emptyRow
                   ++ zip (makeRow 5) emptyRow
                   ++ zip (makeRow 4) emptyRow
                   ++ zip (makeRow 3) emptyRow
                   ++ zip (makeRow 2) (pawnRow White)
                   ++ zip (makeRow 1) (makeFirstRow White)


{-------------------------- Game -------------------------}
type alias Graveyard = List (Maybe Figure)


type alias Player =
  { color     : Color
  , graveyard : Graveyard
  }

type alias Winner = Color

type State = Origin
           | Destination Position
           | Promotion Position
           | CheckMate
           | Finished Winner

type alias Game =
  { board         : Board
  , graveyard1    : Graveyard
  , graveyard2    : Graveyard
  , turn          : Color
  , state         : State
  , timeInSeconds : Int
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
    , timeInSeconds  = 0
    }
