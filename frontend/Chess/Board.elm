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
  [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H' ]


getSquareContent : Board -> Position -> Square
getSquareContent board =
  Maybe.Extra.join << ( flip Dict.get ) board


emptyRow : List ( Maybe a )
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let pawnRow pawnColor = List.repeat 8
        <| Just
        <| piece Pawn pawnColor


      makePiece pieceColor figure =
          Just <| piece figure pieceColor


      makeFirstRow color = List.map
        ( makePiece color )
        [ Rook, Knight, Bishop , Queen
        , King, Bishop, Knight, Rook
        ]


      zip = List.map2 (,)


      makeRow number = zip
        letters
        (List.repeat 8 number)


  in Dict.fromList <| zip ( makeRow 8 ) ( makeFirstRow Black )
                   ++ zip ( makeRow 7 ) ( pawnRow Black )
                   ++ zip ( makeRow 6 ) emptyRow
                   ++ zip ( makeRow 5 ) emptyRow
                   ++ zip ( makeRow 4 ) emptyRow
                   ++ zip ( makeRow 3 ) emptyRow
                   ++ zip ( makeRow 2 ) ( pawnRow White )
                   ++ zip ( makeRow 1 ) ( makeFirstRow White )

shift : Position -> Range -> Position
shift ( char, number ) ( x, y ) =
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
    ( shiftedChar, number + y )


getHorizontalAdjacentPositions : Position -> ( Position, Position )
getHorizontalAdjacentPositions position =
    ( shift position ( 1, 0 ), shift position ( -1, 0 ) )

positionAhead : Color -> Position -> Position
positionAhead color position =
  case color of
    White ->
      shift position ( 0, 1 )
    Black ->
      shift position ( 0, -1 )

positionBelow : Color -> Position -> Position
positionBelow color position =
  case color of
    White ->
      shift position ( 0, -1 )
    Black ->
      shift position ( 0, 1 )

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
takeWhileInclusive predicate xs =
  case xs of
    [] -> []
    (x::xs') ->
      x :: if predicate x
           then takeWhileInclusive predicate xs'
           else []

getRookInitialPosition : Color -> (Position, Position)
getRookInitialPosition turn =
  case turn of
    Black ->
      ( ( 'A', 8 ), ( 'H', 8 ) )

    White ->
       ( ( 'A', 1 ), ( 'H', 1 ) )

getCastlingIntermediatePositions :
  Color -> ( ( Position, Position ), ( Position, Position ) )
getCastlingIntermediatePositions turn =
  case turn of
    Black ->
      ( ( ( 'B', 8 ), ( 'C', 8 ) ), ( ( 'F', 8 ), ( 'G', 8 ) ) )

    White ->
      ( ( ( 'B', 1 ), ( 'C', 1 ) ), ( ( 'F', 1 ), ( 'G', 1 ) ) )



filterPositions : List Position -> List Position
filterPositions positions =
  let
    filterPosition pos =
    -- excludes ranges with ! and negative values
      ( List.member ( fst pos ) letters ) &&
        ( List.member ( snd pos ) [ 1 .. 8 ] )
  in
    List.filter filterPosition <| positions


rangeToSquare : Position -> Board -> Range -> Square
rangeToSquare position board =
  (getSquareContent board) << shift position


isPopulated : Board -> Position -> Range -> Bool
isPopulated board position =
  isJust << rangeToSquare position board


getRegularDestinations : Color -> Board -> Piece -> Position -> List Position
getRegularDestinations turn board piece position =
  let
    zeros = List.repeat 7 0

    oneToSeven = [ 1 .. 7 ]

    negativeOneToSeven = List.reverse [ -7 .. -1 ]

    takeWhileEmpty : List Range -> List Range
    takeWhileEmpty rangesInclusive =
      takeWhileInclusive
        (isNothing << rangeToSquare position board)
          rangesInclusive

--    takeWhileEmpty' a b =
--      takeWhileEmpty <| zip a b
--
--    rookMoves =
--      ( takeWhileEmpty' oneToSeven zeros         ) ++
--      ( takeWhileEmpty' negativeOneToSeven zeros ) ++
--      ( takeWhileEmpty' zeros oneToSeven         ) ++
--      ( takeWhileEmpty' zeros negativeOneToSeven )
--
--    bishopMoves =
--      ( takeWhileEmpty' oneToSeven oneToSeven                ) ++
--      ( takeWhileEmpty' negativeOneToSeven oneToSeven        ) ++
--      ( takeWhileEmpty' oneToSeven negativeOneToSeven        ) ++
--      ( takeWhileEmpty' negativeOneToSeven negativeOneToSeven)
--    takeWhileEmpty' a b =
--      takeWhileEmpty <| zip a b

    takeWhileEmpty' : (List Int , List Int) -> List (Int, Int)
    takeWhileEmpty' (a, b) =
      takeWhileEmpty <| zip a b

    rookMoves =
      ( takeWhileEmpty' (oneToSeven, zeros)         ) ++
      ( takeWhileEmpty' (negativeOneToSeven, zeros) ) ++
      ( takeWhileEmpty' (zeros, oneToSeven)         ) ++
      ( takeWhileEmpty' (zeros, negativeOneToSeven) )

    bishopMoves =
      ( takeWhileEmpty' (oneToSeven, oneToSeven)                ) ++
      ( takeWhileEmpty' (negativeOneToSeven, oneToSeven)        ) ++
      ( takeWhileEmpty' (oneToSeven, negativeOneToSeven)        ) ++
      ( takeWhileEmpty' (negativeOneToSeven, negativeOneToSeven))

    kingMoves =
      [ (  0,  1 ) , (  1,  1 ) , (  1,  0 )
      , (  1, -1 ) , (  0, -1 ) , ( -1, -1 )
      , ( -1,  0 ) , ( -1,  1 )
      ]

    ranges =
      case piece.figure of
        Rook ->
          rookMoves

        Bishop ->
          bishopMoves

        Knight ->
          [ (  1,  2 ) , ( -1,  2 ) , (  2,  1 )
          , (  2, -1 ) , (  1, -2 ) , ( -1, -2 )
          , ( -2,  1 ) , ( -2, -1 )
          ]

        King ->
          kingMoves

        Queen ->
          bishopMoves ++ rookMoves

        Pawn ->
          let
            verticalDestinations oneSquareAhead twoSquaresAhead =
              if isPopulated board position oneSquareAhead
              then []
              else
                if isPopulated board position twoSquaresAhead
                then [ oneSquareAhead ]
                else
                  if piece.moved
                      then [ oneSquareAhead ]
                      else [ oneSquareAhead, twoSquaresAhead ]
          in
              case piece.color of
                White ->
                  verticalDestinations ( 0, 1 ) ( 0, 2 )

                Black ->
                  verticalDestinations ( 0, -1 ) ( 0, -2 )



  in
    filterPositions <| List.map ( shift position ) ranges


charToNum : Char -> Maybe Int
charToNum char =
    Maybe.map snd <|
      List.Extra.find
        ( \ ( char', _ ) -> char' == char )
        [ ( 'A', 1 ) , ( 'B', 2 ) , ( 'C', 3 )
        , ( 'D', 4 ) , ( 'E', 5 ) , ( 'F', 6 )
        , ( 'G', 7 ) , ( 'H', 8 )
        ]


numToChar : Int -> Maybe Char
numToChar num =
    Maybe.map snd <|
      List.Extra.find
        ( \ ( num', _ ) -> num' == num )
        [ ( 1, 'A' ) , ( 2, 'B' ) , ( 3, 'C' )
        , ( 4, 'D' ) , ( 5, 'E' ) , ( 6, 'F' )
        , ( 7, 'G' ) , ( 8, 'H' )
        ]
