import Html exposing (div, button, text)
import StartApp

import String exposing (dropLeft)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


main =
  StartApp.start { model = board, view = drawBoard, update = (\x -> x) }

type alias Position = {x: Float, y: Float}
type alias Piece = { img : String, pos: Position}
type alias Board = { pieces: List (Piece), width: Float, height: Float }

model = "Hello"
board : Board
board = { 
    pieces = makePieces 5 5,
    width = 500,
    height = 500
  }

pieceSize = 50

genBoard x y = if
  | x < 0 || y < 0 -> []
  | x == 0 -> List.map (\y -> (0, y)) [1..y]
  | y == 0 -> List.map (\x -> (x, 0)) [1..x]
  | otherwise -> 
    (genBoard x 0) ++ 
      (genBoard 0 y) ++ 
        List.map2 (\x y -> (x, y)) [0..x] [0..y]

makePiece : Float -> Float -> Piece
makePiece x y = Piece "graphics/brick.jpg" {x = x, y = y}

makePieces : Float -> Float -> List Piece
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece x y) <| genBoard x y


drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces

genRect board piece =
  let 
    startWidth = board.width / 2 - pieceSize
    startHeight = board.height / 2 - pieceSize
  in 
    image pieceSize pieceSize piece.img
      |> toForm
      |> move (toFloat pieceSize * piece.pos.x - startWidth, toFloat pieceSize * piece.pos.y - startHeight)


drawBoard address board =
  Html.fromElement <| 
    collage (round board.width) (round board.height) <| List.map (genRect board) board.pieces
