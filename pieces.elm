module Pieces (WorldObject(Piece, Player), makePiece) where
import Coords exposing (Position)


type WorldObject = 
    Piece { img : String, pos: Position} | 
    Player { img : String, pos: Position}

makePiece : Int -> Int -> WorldObject
makePiece x y =
    let 
        img = if (x + y) % 2 == 0 then "block1.png" else "block2.png"
    in 
        Piece {img = "graphics/" ++ img, pos = {x = x, y = y}}