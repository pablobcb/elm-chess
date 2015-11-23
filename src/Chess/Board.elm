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

-- no list comprehension :(
getPositions : List Position
getPositions =
  List.concat <|
      List.map ( \ digit ->
        List.map ( \ letter->
           ( letter, digit )
        ) letters
      ) [1 .. 8]


getSquareContent : Board -> Position -> Square
getSquareContent board =
  Maybe.Extra.join << ( flip Dict.get ) board


emptyRow : List ( Maybe a )
emptyRow = List.repeat 8 Nothing


makeInitialBoard : Board
makeInitialBoard =
  let
    pawnRow pawnColor =
      List.repeat 8 <| Just <| piece Pawn pawnColor

    makeFirstRow color =
      List.map
        ( Just << ( flip piece ) color )
        [ Rook, Knight, Bishop , Queen
        , King, Bishop, Knight, Rook
        ]


    makeRow number = List.Extra.zip
      letters
      ( List.repeat 8 number )

    zip' ( a, b ) = List.Extra.zip a b

  in
    Dict.fromList <| List.concat <| List.map zip'
      [ ( makeRow 8, makeFirstRow White )
      , ( makeRow 7, pawnRow White      )
      , ( makeRow 6, emptyRow           )
      , ( makeRow 5, emptyRow           )
      , ( makeRow 4, emptyRow           )
      , ( makeRow 3, emptyRow           )
      , ( makeRow 2, pawnRow Black      )
      , ( makeRow 1, makeFirstRow Black )
      ]

shift : Position -> Range -> Position
shift ( char, number ) ( x, y ) =
  let
    charNumericalRepresentation =
      case charToNum char of
        Just num ->
          num

        Nothing ->
          -9999

    shiftedCharNumericalRepresentation =
      charNumericalRepresentation + x

    shiftedChar =
      case numToChar shiftedCharNumericalRepresentation of
        Just char ->
          char

        Nothing ->
          '!'

  in
    ( shiftedChar, number + y )


positionLeft : Position -> Position
positionLeft position =
  shift position ( 1, 0 )


positionRight : Position -> Position
positionRight position =
  shift position ( -1, 0 )


positionAhead : Color -> Position -> Position
positionAhead color position =
  case color of
    Black ->
      shift position ( 0, 1 )
    White ->
      shift position ( 0, -1 )


positionBelow : Color -> Position -> Position
positionBelow color position =
  case color of
    Black ->
      shift position ( 0, -1 )
    White ->
      shift position ( 0, 1 )


-- because pawns take pieces in a different
-- way from how they move, this function is necessary
pawnTakeRanges : Color -> { left : Range, right : Range }
pawnTakeRanges color =
  case color of
     Black ->
       { right = (1, 1), left = (-1, 1) }

     White ->
       { right = (1, -1), left = (-1, -1) }


getLeftRookInitialPosition : Color -> Position
getLeftRookInitialPosition turn =
  case turn of
    Black ->
      ( 'A', 1 )

    White ->
      ( 'A', 8 )


getRightRookInitialPosition : Color -> Position
getRightRookInitialPosition turn =
  case turn of
    Black ->
      ( 'H', 1 )

    White ->
      ( 'H', 8 )


getCastlingIntermediatePositions : Color -> ( List Position, List Position )
getCastlingIntermediatePositions turn =
  case turn of
    Black ->
      ( [('B', 1 ), ( 'C', 1 ), ( 'D', 1 ) ], [( 'F', 1 ), ( 'G', 1 ) ] )

    White ->
      ( [('B', 8 ), ( 'C', 8 ), ( 'D', 8 ) ], [( 'F', 8 ), ( 'G', 8 ) ] )


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


takeWhileInclusive : (a -> Bool) -> List a -> List a
takeWhileInclusive predicate xs =
  case xs of
    [] -> []
    (x::xs') ->
      x :: if predicate x
           then takeWhileInclusive predicate xs'
           else []


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

    takeWhileEmpty' (a, b) =
      takeWhileEmpty <| zip a b

    getDirectionalMoves directions =
      List.concat <| List.map takeWhileEmpty' directions

    rookMoves =
      getDirectionalMoves
        [ (oneToSeven, zeros)
        , (negativeOneToSeven, zeros)
        , (zeros, oneToSeven)
        , (zeros, negativeOneToSeven)
        ]

    bishopMoves =
      getDirectionalMoves
        [ (oneToSeven, oneToSeven)
        , (negativeOneToSeven, oneToSeven)
        , (oneToSeven, negativeOneToSeven)
        , (negativeOneToSeven, negativeOneToSeven)
        ]

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
            isPopulated' = isPopulated board position

            verticalDestinations oneSquareAhead twoSquaresAhead =
              if isPopulated' oneSquareAhead
              then []
              else
                if isPopulated' twoSquaresAhead
                then [ oneSquareAhead ]
                else
                  if piece.moved
                      then [ oneSquareAhead ]
                      else [ oneSquareAhead, twoSquaresAhead ]
          in
              case piece.color of
                Black ->
                  verticalDestinations ( 0, 1 ) ( 0, 2 )

                White ->
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
