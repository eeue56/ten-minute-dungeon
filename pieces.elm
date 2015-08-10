module Pieces (Piece, Player, makePiece, makePlayer) where
import Coords exposing (Position)

type alias Piece = { img : String, pos: Position}
type alias Player = { img : String, pos: Position}

makePiece : Int -> Int -> Piece
makePiece x y =
    let 
        img = if (x + y) % 2 == 0 then "block1.png" else "block2.png"
    in 
        {img = "graphics/" ++ img, pos = {x = x, y = y}}

makePlayer : Int -> Int -> Player
makePlayer x y = {img = "graphics/player.jpg", pos = {x=x, y=y}} 