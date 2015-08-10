module Pieces (Piece, Player, makePiece, makePieces, makePlayer) where

import Coords exposing (Position, genBoard)

type alias Piece = { img : String, pos: Position}
type alias Player = { img : String, pos: Position}

makePiece : Int -> Int -> Piece
makePiece x y =
    let 
        img = if (x + y) % 2 == 0 then "block1.png" else "block2.png"
    in 
        {img = "graphics/" ++ img, pos = {x = x, y = y}}

makePieces : Int -> Int -> List Piece
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece (x) (y) ) <| genBoard x y

makePlayer : Int -> Int -> Player
makePlayer x y = {img = "graphics/player.jpg", pos = {x=x, y=y}} 