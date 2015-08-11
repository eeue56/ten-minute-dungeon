import Pieces exposing (..)
import Coords exposing (..)
import Board exposing (..)
import Drawing exposing (..)
import Signal exposing (..)
import Input exposing (..)

input = Input <~ playerDirection

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



model =
  Signal.foldp
    update
    board
    input

main = drawBoard <~ model