
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
import Maybe exposing ( Maybe(..) )
import Array exposing ( Array(..) )


type Color = Black 
           | White
           
other color =
  case color of
    Black -> White
    White -> Black


type alias Player = Color


type Piece = Pawn 
           | Knight 
           | Bishop 
           | Rook 
           | Queen 
           | King


type alias PieceInstance = 
    { piece : Piece
    , color : Color
    , moved : Bool
    }


type alias Square = 
    { color : Color
    , piece : Maybe PieceInstance
    }


type alias Row = Array Square

type alias Board = Array Row

boardStyle : Attribute
boardStyle =
  style [ ("width" , "640px")
        , ("height", "640px")
        , ("margin", "0.5em")
        , ("border", "2px solid #000")
        ]

squareStyle : Color -> Attribute
squareStyle color =
  let bgColor = case color of
        Black -> ("background-color", "#808080")
        White -> ("background-color", "#0000")
  
  in style <| bgColor :: [ ("float", "left")
                         , ("width", "80px")
                         , ("height", "80px")
                         , ("border", "2px solid #000")
                         ]

blackSquareStyle = squareStyle Black
whiteSquareStyle = squareStyle White 
        
--Square : Square -> Html
--makeSquare square = td

main =
  table [ id "chessBoard", colspan 8, rowspan 8, boardStyle ]
        [ tr [] 
          [ td [ id "A8", blackSquareStyle ] []
          , td [ id "B8", whiteSquareStyle ] []
          , td [ id "C8", blackSquareStyle ] []
          , td [ id "D8", whiteSquareStyle ] []
          , td [ id "E8", blackSquareStyle ] []
          , td [ id "F8", whiteSquareStyle ] []
          , td [ id "G8", blackSquareStyle ] []
          , td [ id "H8", whiteSquareStyle ] []
          ]
        , tr []
          [ td [ id "A7" ] []
          , td [ id "B7" ] []
          , td [ id "C7" ] []
          , td [ id "D7" ] []
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
