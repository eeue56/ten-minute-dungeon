module Board (Board) where
import Pieces exposing (Piece)

type alias Board = { 
    pieces: List (Piece), 
    width: Float, 
    height: Float 
}