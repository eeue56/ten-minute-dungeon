module Drawing (drawPlayer, drawPiece) where
import Graphics.Collage exposing (toForm, move, Form)
import Graphics.Element exposing (image)

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