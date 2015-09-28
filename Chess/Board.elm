module Chess.Board where

import Debug       exposing (..)

import Dict        exposing (..)
import Maybe       exposing (..)
import Maybe.Extra exposing (..)

import List.Extra  exposing (..)

import Chess.Color exposing (..)
import Chess.Piece exposing (..)


type alias Position = (Char, Int)


type alias Square = Maybe Piece


type alias Board = Dict Position Square


letters: List Char
letters =
  ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']


getSquareContent : Board -> Position -> Square
getSquareContent board =
  Maybe.Extra.join << (flip Dict.get) board


emptyRow : List (Maybe a)
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = List.repeat 8
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
        letters
        (List.repeat 8 number)


  in Dict.fromList <| zip (makeRow 8) (makeFirstRow Black)
                   ++ zip (makeRow 7) (pawnRow Black)
                   ++ zip (makeRow 6) emptyRow
                   ++ zip (makeRow 5) emptyRow
                   ++ zip (makeRow 4) emptyRow
                   ++ zip (makeRow 3) emptyRow
                   ++ zip (makeRow 2) (pawnRow White)
                   ++ zip (makeRow 1) (makeFirstRow White)

shift : Position -> Range -> Position
shift (char, number) (x, y) =
  let
    charNumericalRepresentation =
      case charToNum char of
        Just num ->
          num

        Nothing ->
          -10000 --fatal error?

    shiftedCharNumericalRepresentation =
      charNumericalRepresentation + x

    shiftedChar =
      case numToChar shiftedCharNumericalRepresentation of
        Just char ->
          char

        Nothing ->
          '!' --fatal error?

  in
    (shiftedChar, number + y)

positionAhead : Color -> Position -> Position
positionAhead color position =
  case color of
    White ->
      shift position (0, 1)
    Black ->
      shift position (0, -1)

-- because pawns take pieces in a different
-- way from how they move, this function is necessary
pawnTakeRanges : Color -> { left : Range, right : Range }
pawnTakeRanges color =
  case color of
     White ->
       { right = (1, 1), left = (-1, 1) }

     Black ->
       { right = (1, -1), left = (-1, -1) }

takeWhileInclusive : (a -> Bool) -> List a -> List a
takeWhileInclusive p xs' =
  case xs' of
    x::xs -> x :: if p x
                  then takeWhileInclusive p xs
                  else []
    [] -> []

getRegularMoves : Color -> Board -> Piece -> Position -> List Position
getRegularMoves turn  board piece position =
  let
    filterPosition pos =
    -- excludes ranges with ! and negative values
      (List.member (fst pos) letters) &&
        (List.member (snd pos) [1..8])


    ranges =
      let
        zip = List.map2 (,)

        zeros = List.repeat 7 0

        oneToSeven = [ 1 .. 7 ]

        negativeOneToSeven =
          List.map ( (*) (-1)) oneToSeven

        rangeToSquare : Range -> Square
        rangeToSquare = (getSquareContent board) << shift position

      --  takeWhileEmpty : List Range -> List Range
      --  takeWhileEmpty ranges =
      --    List.Extra.takeWhile (isNothing << rangeToSquare) ranges

        takeWhileEmpty : List Range -> List Range
        takeWhileEmpty rangesInclusive =
          takeWhileInclusive (\range ->
            case watch "square" <| rangeToSquare range of
              Just piece ->
                if piece.color == turn
                then False
                else True
              Nothing -> True
          ) ranges

        antes  = watch "antes" <| zip oneToSeven zeros
        depois  = watch "depois" <| takeWhileEmpty <| zip oneToSeven zeros

        --zipAndTakeEmpty = takeWhileEmpty << zip

        rookMoves =
          (takeWhileEmpty <| zip  oneToSeven zeros) ++
          (takeWhileEmpty <| zip negativeOneToSeven zeros) ++
          (takeWhileEmpty <| zip zeros oneToSeven        ) ++
          (takeWhileEmpty <| zip zeros negativeOneToSeven)

        bishopMoves =
          (takeWhileEmpty <| zip oneToSeven oneToSeven        ) ++
          (takeWhileEmpty <| zip negativeOneToSeven oneToSeven) ++
          (takeWhileEmpty <| zip oneToSeven negativeOneToSeven) ++
          (takeWhileEmpty <| zip negativeOneToSeven negativeOneToSeven)

        kingMoves =
          [ (  0,  1 )
          , (  1,  1 )
          , (  1,  0 )
          , (  1, -1 )
          , (  0, -1 )
          , ( -1, -1 )
          , ( -1,  0 )
          , ( -1,  1 )
          ]

      in
        case piece.figure of
          Pawn ->
            case piece.color of
              White ->
                [(0, 1)] ++
                  if piece.moved
                      then []
                      else [(0, 2)]

              Black ->
                [(0, -1)] ++
                  if piece.moved
                  then []
                  else [(0, -2)]

          Rook ->
            rookMoves

          Bishop ->
            bishopMoves

          Knight ->
            [ (  1,  2 )
            , ( -1,  2 )
            , (  2,  1 )
            , (  2, -1 )
            , (  1, -2 )
            , ( -1, -2 )
            , ( -2,  1 )
            , ( -2, -1 )
            ]

          King ->
            kingMoves

          Queen ->
            bishopMoves
            ++ rookMoves

  in
    List.filter filterPosition <|
    List.map (shift position) ranges

charToNum : Char -> Maybe Int
charToNum char =
  if | char == 'A' -> Just 1
     | char == 'B' -> Just 2
     | char == 'C' -> Just 3
     | char == 'D' -> Just 4
     | char == 'E' -> Just 5
     | char == 'F' -> Just 6
     | char == 'G' -> Just 7
     | char == 'H' -> Just 8
     | otherwise   -> Nothing

numToChar : Int -> Maybe Char
numToChar num =
  if | num == 1  -> Just 'A'
     | num == 2  -> Just 'B'
     | num == 3  -> Just 'C'
     | num == 4  -> Just 'D'
     | num == 5  -> Just 'E'
     | num == 6  -> Just 'F'
     | num == 7  -> Just 'G'
     | num == 8  -> Just 'H'
     | otherwise -> Nothing
