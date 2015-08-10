module Pieces (Piece, makePiece) where
import Coords exposing (Position)

type alias Piece = { img : String, pos: Position}

makePiece : Float -> Float -> Piece
makePiece x y = Piece "graphics/brick.jpg" {x = x, y = y}