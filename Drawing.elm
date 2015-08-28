module Drawing (drawPlayer, drawPiece, drawBoard) where

import Html exposing (div, button, text)

import Graphics.Collage exposing (toForm, move, Form, rect, filled, collage)
import Graphics.Element exposing (image, layers, show)
import Color exposing (red, black, toRgb, rgb)

import Board exposing (Board)
import Pieces exposing (..)


drawPlayer : Board -> Player -> Form
drawPlayer board player = 
  let 
    pieceSize = board.pieceSize
    originX = pieceSize/2-(board.width / 2) 
    originY = pieceSize/2-(board.height / 2)
    x = originX + pieceSize * (toFloat player.pos.x)
    y = originY + pieceSize * (toFloat player.pos.y)
  in
  image (round pieceSize) (round pieceSize) player.img
    |> toForm
    |> move (x, y)

drawPiece board piece = 
  let 
    pieceSize = board.pieceSize
    originX = pieceSize/2-(board.width / 2) 
    originY = pieceSize/2-(board.height / 2)
    x = originX + pieceSize * (toFloat piece.pos.x)
    y = originY + pieceSize * (toFloat piece.pos.y)
  in
    image (round pieceSize) (round pieceSize) piece.img
      |> toForm
      |> move (x, y)

drawBoard board =
  div [] 
  [
    Html.fromElement <| 
      layers 
        [
         (boardCollage board <| [rect board.width board.height |> filled black]),
         (boardCollage board <| List.map (drawPiece board) board.pieces),
         (boardCollage board <| List.map (drawPiece board) board.trail),
         (boardCollage board <| [drawPlayer board board.player])]
    , Html.fromElement <| show board
  ]

drawBoardCoords address board = 
  div [] <| List.map (\piece -> button [] [Html.text <| toString [piece.pos.x, piece.pos.y]]) <| board.pieces


boardCollage board = collage (round board.width) (round board.height)
