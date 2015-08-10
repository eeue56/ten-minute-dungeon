module Drawing (drawWorldObject) where
import Graphics.Collage exposing (toForm, move, Form)
import Graphics.Element exposing (image)

import Board exposing (Board)
import Pieces exposing (..)


drawWorldObject : Board -> WorldObject -> Form
drawWorldObject board object = 
    let
        pieceSize = board.pieceSize
        startWidth = board.width / 2 - pieceSize
        startHeight = board.height / 2 - pieceSize
    in 
        case object of 
            Piece piece -> 
                let 
                    x = pieceSize * (toFloat piece.pos.x) - startWidth
                    y = pieceSize * (toFloat piece.pos.y) - startHeight
                in
                image (round pieceSize) (round pieceSize) piece.img
                        |> toForm
                        |> move (x, y)

            Player player -> 
                let 
                    x = pieceSize * (toFloat player.pos.x) - startWidth
                    y = pieceSize * (toFloat player.pos.y) - startHeight
                in
                image (round pieceSize) (round pieceSize) player.img
                        |> toForm
                        |> move (x, y)