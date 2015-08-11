module Board (Board, update) where

import Focus exposing ((=>), create, Focus)

import Pieces exposing (Player, Piece, makePlayer)
import Input exposing (..)
import Coords exposing (..)




type alias Board = { 
    player: Player,
    pieces: List (Piece), 
    width: Float, 
    height: Float,
    pieceSize: Float,
    maxX: Int,
    maxY: Int
}

move maxY y = if 
    | y < 0 -> 0
    | y > maxY -> maxY
    | otherwise -> y


update : Input -> Board -> Board
update action board = 
  let 
    bindX = move board.maxX 
    bindY = move board.maxY
  in
    case action.direction of 
      None -> board
      Up ->  Focus.update playerY (\y -> bindY <| y + 1) board
      Down -> Focus.update playerY (\y -> bindY <| y - 1) board
      Right ->  Focus.update playerX (\x -> bindX <| x + 1) board
      Left -> Focus.update playerX (\x -> bindX <| x - 1) board
      otherwise -> board

pieceSize : Focus { record | pieceSize : a} a
pieceSize = create .pieceSize (\f r -> { r | pieceSize <- f r.pieceSize })

playerX = player => pos => x
playerY = player => pos => y

player = create .player (\f r -> { r | player <- f r.player })
img = create .img (\f r -> { r | img <- f r.img })
pos = create .pos (\f r -> { r | pos <- f r.pos })
y = create .y (\f r -> { r | y <- f r.y })
x = create .x (\f r -> { r | x <- f r.x })