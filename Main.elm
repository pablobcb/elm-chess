
--import Counter exposing (update, view)
--import StartApp.Simple exposing (start)


--main =
  --start
--    { model = 0
--    , update = update
--    , view = view
--    }
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing  ( Maybe(..) )
import Array exposing  ( Array(..) )
import Text exposing   ( Text(..) )
import String exposing ( fromChar )

import Board exposing (..)


----------------------------- Style --------------------------------           


boardStyle : Attribute
boardStyle =
    style [ ("width" , "640px")
        , ("height", "640px")
        , ("margin", "0.5em")
        , ("border", "1px solid #000")
        ]

squareStyle : Color -> Attribute
squareStyle color =
  let bgColor = case color of
        Black -> ("background-color", "#808080")
        White -> ("background-color", "#0000")
  
  in style <| bgColor :: [ ("float", "left")
                         , ("width", "80px")
                         , ("height", "80px")
                         --, ("border", "1px solid #000")
                         , ("font-size", "400%")
                         , ("text-align", "center")
                         , ("vertical-align", "middle")
                         ]


--main = text (String.fromChar '\x2658')

renderPiece piece = text <| String.fromChar <| 
  case piece.figure of
    King -> case piece.color of
      Black -> '\x265A'
      White -> '\x2654'

    Queen -> case piece.color of
      Black -> '\x265B'
      White -> '\x2655'

    Rook -> case piece.color of
      Black -> '\x265C'
      White -> '\x2656'

    Bishop -> case piece.color of
      Black -> '\x265D'
      White -> '\x2657'

    Knight -> case piece.color of
      Black -> '\x265E'
      White -> '\x2658'

    Pawn -> case piece.color of
      Black -> '\x265F'
      White -> '\x2659'
main =
  table [ id "chessBoard"]
        [ tr [] 
          [ td [ id "A8", squareStyle Black ] [ renderPiece <| piece King   Black False ]
          , td [ id "B8", squareStyle White ] [ renderPiece <| piece King   White False ]
          , td [ id "C8", squareStyle Black ] [ renderPiece <| piece Queen  Black False ]
          , td [ id "D8", squareStyle White ] [ renderPiece <| piece Queen  White False ]
          , td [ id "E8", squareStyle Black ] [ renderPiece <| piece Bishop Black False ]
          , td [ id "F8", squareStyle White ] [ renderPiece <| piece Bishop White False ]
          , td [ id "G8", squareStyle Black ] [ renderPiece <| piece Knight Black False ]
          , td [ id "H8", squareStyle White ] [ renderPiece <| piece Knight White False ]
          ]
        , tr []
          [ td [ id "A7", squareStyle White ] [ renderPiece <| piece Rook White False ]
          , td [ id "B7", squareStyle Black ] [ renderPiece <| piece Rook Black False ]
          , td [ id "C7", squareStyle White ] [ renderPiece <| piece Pawn Black False ]
          , td [ id "D7", squareStyle Black ] [ renderPiece <| piece Pawn White False ]
          , td [ id "E7" ] []
          , td [ id "F7" ] []
          , td [ id "G7" ] []
          , td [ id "H7" ] []
          ]
        , tr []
          [ td [ id "A6" ] []
          , td [ id "B6" ] []
          , td [ id "C6" ] []
          , td [ id "D6" ] []
          , td [ id "E6" ] []
          , td [ id "F6" ] []
          , td [ id "G6" ] []
          , td [ id "H6" ] []
          ]
        , tr []
          [ td [ id "A5" ] []
          , td [ id "B5" ] []
          , td [ id "C5" ] []
          , td [ id "D5" ] []
          , td [ id "E5" ] []
          , td [ id "F5" ] []
          , td [ id "G5" ] []
          , td [ id "H5" ] []
          ]
        , tr []
          [ td [ id "A4" ] []
          , td [ id "B4" ] []
          , td [ id "C4" ] []
          , td [ id "D4" ] []
          , td [ id "E4" ] []
          , td [ id "F4" ] []
          , td [ id "G4" ] []
          , td [ id "H4" ] []
          ]
        , tr []
          [ td [ id "A3" ] []
          , td [ id "B3" ] []
          , td [ id "C3" ] []
          , td [ id "D3" ] []
          , td [ id "E3" ] []
          , td [ id "F3" ] []
          , td [ id "G3" ] []
          , td [ id "H3" ] []
          ]
        , tr []
          [ td [ id "A2" ] []
          , td [ id "B2" ] []
          , td [ id "C2" ] []
          , td [ id "D2" ] []
          , td [ id "E2" ] []
          , td [ id "F2" ] []
          , td [ id "G2" ] []
          , td [ id "H2" ] []
          ]
        , tr []
          [ td [ id "A1" ] []
          , td [ id "B1" ] []
          , td [ id "C1" ] []
          , td [ id "D1" ] []
          , td [ id "E1" ] []
          , td [ id "F1" ] []
          , td [ id "G1" ] []
          , td [ id "H1" ] []
          ]
        ]
