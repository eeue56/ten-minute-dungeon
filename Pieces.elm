module Pieces (Piece, Player, makePiece, makePieces, makePlayer, makeMaze) where

import Coords exposing (Position, genBoard)

import Set as Set

type alias Piece = { img : String, pos: Position}
type alias Player = { img : String, pos: Position}
type alias Cell = (Int, Int)

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
makeMaze height width = 
  let
    justHead : List a -> a
    justHead x = case List.head x of Just v -> v

    rectangle : Int -> Int -> Set.Set Cell
    rectangle n m = Set.fromList <| List.concat <| List.map (\x-> List.map2 (,) [0..(n-1)] <| List.repeat n x) [0..(m-1)]

    inBounds : Cell -> Bool
    inBounds (i',j') = (i' < height) && (j' < width) && (i' >= 0) && (j' >= 0)

    sets : Set.Set Cell -> List (Set.Set Cell)
    sets maze = 
      let getConnected cell connected = Set.foldl 
                      (\neighbor connected' -> if Set.member neighbor connected' 
                                                then connected' 
                                                else getConnected neighbor <| Set.insert neighbor connected')
                      connected 
                      <| Set.intersect maze <| neighbors cell
      in Set.foldl (\cell acc -> if Set.member cell <| List.foldl Set.union Set.empty acc 
                                  then acc 
                                  else (getConnected cell <| Set.singleton cell)::acc) [] maze
  
    connect : List (Set.Set Cell) -> List (Set.Set Cell)
    connect setList = case setList of
                        (x::[]) -> [x]
                        (x::y::xs) -> case combine x y of
                                        Nothing -> connect <| y :: (connect <| x::xs)
                                        Just s -> connect <| s :: xs

    combine : Set.Set Cell -> Set.Set Cell -> Maybe (Set.Set Cell)
    combine s1 s2 = 
      let 
        shared = Set.intersect (perimeter s1) (perimeter s2)
      in
        if Set.isEmpty shared
          then Nothing 
          else Just <| Set.insert (justHead <| Set.toList shared) <| Set.union s1 s2


    perimeter : Set.Set Cell -> Set.Set Cell
    perimeter component = Set.diff (Set.foldl (\cell acc -> Set.union acc <| neighbors cell) Set.empty component) component

    inverse : Set.Set Cell -> Set.Set Cell
    inverse x = Set.diff allPairs x

    allPairs = rectangle height width
    
    evolve : Set.Set Cell -> Set.Set Cell
    evolve board = 
      let
        numNeighbors cell = List.length <| Set.toList <| Set.intersect board <| allNeighbors cell
      in 
        Set.union 
          (Set.filter (\cell -> numNeighbors cell == 3) allPairs)
          (Set.filter (\cell -> numNeighbors cell < 5 && (numNeighbors cell) >= 1) board)
    --maze =  evolveN 2 start
     
    maze = 
      let
        start = List.foldl Set.union Set.empty [neighbors (2,2), allNeighbors (5,5), diagonals (6,3)]
        evolveN n = List.foldl (>>) identity (List.repeat n evolve)
      in
        Set.toList <| inverse <| justHead <| connect <| sets <| evolveN 15 start
            
    allNeighbors : Cell -> Set.Set Cell
    allNeighbors cell = Set.union (neighbors cell) (diagonals cell)

    neighbors : Cell -> Set.Set Cell
    neighbors (i,j) = Set.fromList <| List.filter inBounds <| List.map (\(di,dj) -> (i+di, j+dj)) [(-1,0), (1,0), (0,-1), (0,1)]

    diagonals : Cell -> Set.Set Cell
    diagonals (i,j) = Set.fromList <| List.filter inBounds <| List.map (\(di,dj) -> (i+di, j+dj)) [(-1,-1), (1,-1), (-1,1), (1,1)]
  in
    List.map (\(x, y) -> makePiece x y ) <| maze

makePlayer : Int -> Int -> Player
makePlayer x y = {img = "graphics/player.jpg", pos = {x=x, y=y}} 
