module Drawing (drawPlayer, drawPiece, drawBoard) where

import Html exposing (div, button, text)

import Graphics.Collage exposing (toForm, move, Form, collage)
import Graphics.Element exposing (image, layers, show)

import Board exposing (Board)
import Pieces exposing (..)


drawPlayer : Board -> Player -> Form
drawPlayer board player = 
  let 
    pieceSize = board.pieceSize
    startWidth = board.width / 2 - pieceSize
    startHeight = board.height / 2 - pieceSize
    x = pieceSize * (toFloat player.pos.x) - startWidth
    y = pieceSize * (toFloat player.pos.y) - startHeight
  in
  image (round pieceSize) (round pieceSize) player.img
    |> toForm
    |> move (x, y)

drawPiece board piece = 
  let 
    pieceSize = board.pieceSize
    startWidth = board.width / 2 - pieceSize
    startHeight = board.height / 2 - pieceSize
    x = pieceSize * (toFloat piece.pos.x) - startWidth
    y = pieceSize * (toFloat piece.pos.y) - startHeight
  in
  image (round pieceSize) (round pieceSize) piece.img
    |> toForm
    |> move (x, y)

drawBoard board =
  div [] 
  [
    Html.fromElement <| 
      layers 
        [(boardCollage board <| List.map (drawPiece board) board.pieces), 
         (boardCollage board <| [drawPlayer board board.player])]
    , Html.fromElement <| show board
  ]

drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces


boardCollage board = collage (round board.width) (round board.height)
