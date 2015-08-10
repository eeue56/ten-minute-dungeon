import Html exposing (div, button, text)
import StartApp

import String exposing (dropLeft)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)

main =
  StartApp.start { model = board, view = drawBoard, update = (\x -> x) }


board : Board
board = { 
    pieces = makePieces 5 5,
    width = 500,
    height = 500
  }

pieceSize = 50

makePieces : Float -> Float -> List Piece
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece x y) <| genBoard x y



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

drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces
