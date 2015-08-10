module Board (Board, update) where

import Pieces exposing (Player, Piece, makePlayer)
import Input exposing (..)
import Coords exposing (..)

import Focus exposing ((=>), create, Focus)



type alias Board = { 
    player: Player,
    pieces: List (Piece), 
    width: Float, 
    height: Float,
    pieceSize: Float,
    maxX: Int,
    maxY: Int
}

move y maxY = if 
    | y < 0 -> 0
    | y > maxY -> maxY
    | otherwise -> y

update : Input -> Board -> Board
update action board = 
  case action.direction of 
    None -> board
    Up ->  Focus.update (player => pos => y) (\y -> move (y + 1) board.maxY) board
    Down -> Focus.update (player => pos => y) (\y -> move (y - 1) board.maxY) board
    Right ->  Focus.update (player => pos => x) (\x -> move (x + 1) board.maxX) board
    Left -> Focus.update (player => pos => x) (\x -> move (x - 1) board.maxX) board
    otherwise -> board

pieceSize : Focus { record | pieceSize : a} a
pieceSize = create .pieceSize (\f r -> { r | pieceSize <- f r.pieceSize })

player = create .player (\f r -> { r | player <- f r.player })
img = create .img (\f r -> { r | img <- f r.img })
pos = create .pos (\f r -> { r | pos <- f r.pos })
y = create .y (\f r -> { r | y <- f r.y })
x = create .x (\f r -> { r | x <- f r.x })