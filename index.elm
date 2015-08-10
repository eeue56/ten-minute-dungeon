import Html exposing (div, button, text)
import String exposing (dropLeft)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Set
import Dict
import Keyboard

import Html.Events exposing (onKeyDown, onClick, onKeyPress)

import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)
import Drawing exposing (..)
import Signal exposing (..)

type alias App model action =
    { model : model
    , view : Signal.Address action -> model -> Html.Html
    , update : action -> model -> model
    }

start : App model action -> Signal Html.Html
start app =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    model =
      Signal.foldp
        (\(Just action) model -> app.update action model)
        app.model
        actions.signal
  in
    (app.view address) <~ (model) 

main = start { model = board, view = drawBoard, update = update }

board : Board
board = {
    player = makePlayer 5 5,
    pieces = makePieces 8 8,
    width = 500,
    height = 500,
    pieceSize = 50
  }


type Action = None | Dogs Int


makePlayer : Int -> Int -> WorldObject
makePlayer x y = Player {pos = {x=x, y=y}, img = "graphics/player.jpg"} 


makePieces : Int -> Int -> List WorldObject
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece (x) (y) ) <| genBoard x y

genRect = drawWorldObject

boardCollage board = collage (round board.width) (round board.height)

drawBoard : Signal.Address Action -> Board -> Html.Html
drawBoard address board =
  div [onClick address <| Dogs 87]
  [
    Html.fromElement <| 
      layers [(boardCollage board <| List.map (genRect board) board.pieces), (boardCollage board <| [genRect board board.player])]
    ]

drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces

updateBoard : Board -> Bool -> Board
updateBoard board isDown = if isDown then { board - player | player = makePlayer 3 3 } else board

update : Action -> Board -> Board
update action board = 
  case action of 
    None -> board
    Dogs key -> if key == 87 then updateBoard board True else board

pressed : Signal Bool
pressed = Keyboard.isDown 87