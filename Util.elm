module Util (dimUtils, justHead, rectangle) where

import Set as Set
import Dict as Map

type alias Cell = (Int, Int)


justHead : List a -> a
justHead x = case List.head x of Just v -> v

rectangle : Int -> Int -> Set.Set Cell
rectangle n m = Set.fromList <| List.concat <| List.map (\x-> List.map2 (,) [0..(n-1)] <| List.repeat n x) [0..(m-1)]

dimUtils height width = 
  let
    allPairs = rectangle height width

    inBounds : Cell -> Bool
    inBounds (i',j') = (i' < height) && (j' < width) && (i' >= 0) && (j' >= 0)

    perimeter : Set.Set Cell -> Set.Set Cell
    perimeter component = Set.diff (Set.foldl (\cell acc -> Set.union acc <| neighbors cell) Set.empty component) component

    inverse : Set.Set Cell -> Set.Set Cell
    inverse x = Set.diff allPairs x

    allNeighbors : Cell -> Set.Set Cell
    allNeighbors cell = Set.union (neighbors cell) (diagonals cell)

    neighbors : Cell -> Set.Set Cell
    neighbors (i,j) = 
      Set.fromList 
        <| List.filter inBounds 
        <| 
          [(-1 + i, j), 
           (1 + i, j),
           (i, j - 1), 
           (i, j + 1)]

    diagonals : Cell -> Set.Set Cell
    diagonals (i,j) = 
      Set.fromList 
        <| List.filter inBounds 
        <| 
          [(i - 1, j - 1), 
           (i + 1, j - 1), 
           (i - 1, j + 1), 
           (i + 1, j + 1)]
  
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
          else case List.head <| Set.toList shared of
            Just x -> Just <| Set.insert x <| Set.union s1 s2
            Nothing -> Nothing

    evolve : Set.Set Cell -> Set.Set Cell
    evolve board = 
      let
        numNeighbors cell = List.length <| Set.toList <| Set.intersect (allNeighbors cell) board
        allPairsNeighbors = Set.map (\cell -> (cell, numNeighbors cell)) allPairs
        boardNeighbors = Set.filter (\(cell,_) -> Set.member cell board) allPairsNeighbors
      in 
        Set.map fst <| Set.union 
                        (Set.filter (\(_,n) -> n == 3) allPairsNeighbors)
                        (Set.filter (\(_,n) -> n < 5 && n >= 1) boardNeighbors)
    maze = 
      let
        start = List.foldl Set.union Set.empty [neighbors (2,2), allNeighbors (5,5), diagonals (6,3)]
        evolveN n = List.foldl (>>) identity (List.repeat n evolve)
      in
        inverse <| justHead <| connect <| sets <| evolveN 10 start

  in 
    { inBounds=inBounds, 
      perimeter=perimeter, 
      inverse=inverse, 
      allPairs=allPairs, 
      allNeighbors=allNeighbors, 
      neighbors=neighbors, 
      diagonals=diagonals, 
      maze=maze}