module Pieces (Piece, Player, makePiece, makePieces, makePlayer, makeMaze, makeTrail) where

import Coords exposing (Position, genBoard)
import Util

import Set as Set
import Dict as Dict

type alias Piece = { img : String, pos: Position}
type alias Player = { img : String, pos: Position}
type alias Cell = (Int, Int)

utils = Util.dimUtils 8 8

makePiece : Int -> Int -> Piece
makePiece x y =
    let 
        img = if (x + y) % 2 == 0 then "block1.png" else "block2.png"
    in 
        {img = "graphics/" ++ img, pos = {x = x, y = y}}

makePieces : Int -> Int -> List Piece
makePieces x y = if
  | x < 0 || y < 0 -> []
  | otherwise -> List.map (\(x, y) -> makePiece x y ) <| genBoard x y

makeMaze : Int -> Int -> List Piece
makeMaze height width = (List.map (\(x, y) -> makePiece x y ) <| Set.toList utils.maze)

makePlayer : Int -> Int -> Player
makePlayer x y = {img = "graphics/player.jpg", pos = {x=x, y=y}} 

makeTrail : Cell -> Cell -> List Piece
makeTrail s e = 
  let
    bfs : Cell -> Cell -> Set.Set Cell -> Maybe (List Cell)
    bfs start end board = 
      let
        --getPath : Cell -> Dict.Dict Cell Cell -> List Cell
        getPath node visited = if
          | node == start -> [start]
          | otherwise -> case Dict.get node visited of Just node' -> getPath node' visited ++ [node]
       -- bfsHelper : Dict.Dict Cell Cell -> List (Cell, Cell) -> Maybe (List Cell)
        bfsHelper visited xs = case xs of
          [] -> Nothing
          ((from, to)::qs) -> if
              | to == end -> Just <| getPath to <| Dict.insert to from visited
              | Dict.member to visited -> bfsHelper visited qs
              | otherwise -> bfsHelper (Dict.insert to from visited) <| qs ++ (List.map (\x -> (to,x)) <| Set.toList <| Set.intersect board <| utils.neighbors to)
      in
        bfsHelper Dict.empty [(start,start)]
  in
    List.map (\(x,y) -> {img = "graphics/blockSheep.png", pos = {x = x, y = y}} ) <| case bfs s e <| utils.inverse utils.maze of 
                                                                                      Just xs -> xs
                                                                                      Nothing -> []