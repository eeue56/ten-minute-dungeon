import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)
import Drawing exposing (..)
import Signal exposing (..)
import Input exposing (..)
import Util exposing (..)

input = Input <~ inputSignal

board : Board
board = 
  let 
    player = makePlayer 5 5
    pieceSize = 50
    cols = 8
    rows = 8
    pieces = makeMaze cols rows
    width = cols * pieceSize
    height = rows * pieceSize
  in 
  {
    player = player,
    pieces = pieces,
    trail = makeTrail (player.pos.x,  player.pos.y) (7,7),
    width = width,
    height = height,
    pieceSize = pieceSize,
    cols = cols,
    rows = rows
  }



model =
  Signal.foldp
    update
    board
    input

main = drawBoard <~ model
