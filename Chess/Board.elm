module Chess.Board where

import Debug       exposing (..)

import Dict        exposing (..)
import Maybe       exposing (..)
import Maybe.Extra exposing (..)

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

getRegularMoves : List Range -> Position -> List Position
getRegularMoves ranges position =
  let
    filterPosition pos =
    -- excludes ranges with ! and negative values
      (List.member (fst pos) letters) &&
        (List.member (snd pos) [1..8])
  in
    List.filter filterPosition <|
    watch "unfiltered" <| List.map (shift position) ranges

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
