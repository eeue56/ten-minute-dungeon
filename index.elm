import Html exposing (div, button, text)
import String exposing (dropLeft)

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Html.Events exposing (onKeyDown, onClick, onKeyPress)

import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)
import Drawing exposing (..)
import Signal exposing (..)
import Input exposing (..)

input = Input <~ playerDirection


model =
  Signal.foldp
    update
    board
    input

main = drawBoard <~ model



board : Board
board = {
    player = makePlayer 5 5,
    pieces = makePieces 8 8,
    width = 500,
    height = 500,
    pieceSize = 50,
    maxX = 8,
    maxY = 8
  }



makePieces : Int -> Int -> List Piece
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece (x) (y) ) <| genBoard x y

boardCollage board = collage (round board.width) (round board.height)


drawBoard board =
  Html.fromElement <| 
    layers 
      [(boardCollage board <| List.map (drawPiece board) board.pieces), 
       (boardCollage board <| [drawPlayer board board.player])]


drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces