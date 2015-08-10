module Pieces (WorldObject(Piece, Player), makePiece) where
import Coords exposing (Position)


type WorldObject = 
    Piece { img : String, pos: Position} | 
    Player { img : String, pos: Position}

makePiece : Int -> Int -> WorldObject
makePiece x y = Piece {img = "graphics/brick.jpg", pos = {x = x, y = y}}


