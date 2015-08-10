module Board (Board) where
import Pieces exposing (WorldObject)

type alias Board = { 
    pieces: List (WorldObject), 
    width: Float, 
    height: Float ,
    pieceSize: Float
}