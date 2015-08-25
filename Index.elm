import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)
import Drawing exposing (..)
import Signal exposing (..)
import Input exposing (..)
import Util exposing (..)

input = Input <~ playerDirection



board : Board
board = 
  let 
    player = makePlayer 5 5 
  in 
  {
    player = player,
    pieces = makeMaze 8 8,
    trail = makeTrail (player.pos.x,  player.pos.y) (7,7),
    width = 500,
    height = 500,
    pieceSize = 50,
    maxX = 8,
    maxY = 8
  }



model =
  Signal.foldp
    update
    board
    input

main = drawBoard <~ model
