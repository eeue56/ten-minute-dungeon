module Util (dimUtils, justHead, rectangle) where

import Set as Set
import Dict as Dict

type alias Cell = (Int, Int)


justHead : List a -> a
justHead x = case List.head x of Just v -> v

justSetHead : List (a,a) -> a
justSetHead x = case List.head x of Just (s,p) -> s

rectangle : Int -> Int -> Set.Set Cell
rectangle n m = Set.fromList <| List.concat <| List.map (\x-> List.map2 (,) [0..(n-1)] <| List.repeat n x) [0..(m-1)]

dimUtils height width = 
  let
    allPairs = rectangle height width

    precalculatedAllNeighbors : Dict.Dict Cell (List Cell)
    precalculatedAllNeighbors = Set.foldl (\cell acc -> Dict.insert cell (allNeighborsList cell) acc) Dict.empty allPairs

    inBounds : Cell -> Bool
    inBounds (i',j') = (i' < height) && (j' < width) && (i' >= 0) && (j' >= 0)

    perimeter : Set.Set Cell -> Set.Set Cell
    perimeter component = Set.diff (Set.foldl (\cell acc -> Set.union acc <| neighbors cell) Set.empty component) component

    inverse : Set.Set Cell -> Set.Set Cell
    inverse x = Set.diff allPairs x

    allNeighbors : Cell -> Set.Set Cell
    allNeighbors cell = Set.union (neighbors cell) (diagonals cell)

    fastAllNeighbors : Cell -> List Cell
    fastAllNeighbors cell = case Dict.get cell precalculatedAllNeighbors of Just v -> v

    allNeighborsList : Cell -> List Cell
    allNeighborsList (i,j) = 
      List.filter inBounds
          [(-1 + i, j), 
           (1 + i, j),
           (i, j - 1), 
           (i, j + 1),
           (i - 1, j - 1), 
           (i + 1, j - 1), 
           (i - 1, j + 1), 
           (i + 1, j + 1)]


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
      let getConnected cell connected = 
        Set.foldl 
            (\neighbor connected' -> 
              if Set.member neighbor connected' 
              then connected' 
              else getConnected neighbor <| Set.insert neighbor connected')
            connected 
            <| Set.intersect maze <| neighbors cell
      in 
        Set.foldl 
          (\cell acc -> 
            if Set.member cell <| List.foldl Set.union Set.empty acc 
            then acc 
            else (getConnected cell <| Set.singleton cell) :: acc) 
          [] maze
  
    connect : List (Set.Set Cell) -> List (Set.Set Cell)
    connect setList = case setList of
                        ([x]) -> [x]
                        (x::y::xs) -> case combine x y of
                                        Nothing -> connect <| y :: (connect <| x::xs)
                                        Just s -> connect <| s :: xs

    connectWithPerimeter : List (Set.Set Cell, Set.Set Cell) -> List (Set.Set Cell, Set.Set Cell)
    connectWithPerimeter setList = case setList of
                        ([x]) -> [x]
                        (x::y::xs) -> case combineWithPerimeter x y of
                                        Nothing -> connectWithPerimeter <| y :: (connectWithPerimeter <| x::xs)
                                        Just s -> connectWithPerimeter <| s :: xs

    combine : Set.Set Cell -> Set.Set Cell -> Maybe (Set.Set Cell)
    combine s1 s2 = 
      let 
        shared = Set.intersect (perimeter s1) (perimeter s2)
      in
        case List.head <| Set.toList shared of
          Just x -> Just <| Set.insert x <| Set.union s1 s2
          Nothing -> Nothing

    combineWithPerimeter : (Set.Set Cell, Set.Set Cell) -> (Set.Set Cell, Set.Set Cell) -> Maybe (Set.Set Cell, Set.Set Cell)
    combineWithPerimeter (s1,p1) (s2,p2) = 
      let 
        shared = Set.intersect p1 p2
      in
        case List.head <| Set.toList shared of
          Just x -> Just (Set.insert x <| Set.union s1 s2, Set.union p1 p2)
          Nothing -> Nothing

    evolve : Set.Set Cell -> Set.Set Cell
    evolve board = 
      let
        numNeighbors cell = List.length <| List.filter (\x -> Set.member x board) <| fastAllNeighbors cell
        allPairsNeighbors = Set.map (\cell -> (cell, numNeighbors cell)) allPairs
        boardNeighbors = Set.filter (\(cell, n) -> Set.member cell board && (n < 5 && n >= 1)) allPairsNeighbors
      in 
        Set.map fst <| Set.union 
                        (Set.filter (\(_,n) -> n == 3) allPairsNeighbors)
                        boardNeighbors

    maze = 
      let
        start = List.foldl Set.union Set.empty [neighbors (2,2), allNeighbors (5,5), diagonals (9,3), neighbors (7,13), allNeighbors (3,13), diagonals (10,1)]
        evolveN n = List.foldl (>>) identity (List.repeat n evolve)
      in
        inverse <| justSetHead <| connectWithPerimeter <| mapPerimeter <| sets <| evolveN 15 start
        
    mapPerimeter : List (Set.Set Cell) -> List (Set.Set Cell, Set.Set Cell)
    mapPerimeter xs = List.map (\x -> (x, perimeter x)) xs

  in 
    { inBounds=inBounds, 
      perimeter=perimeter, 
      inverse=inverse, 
      allPairs=allPairs, 
      allNeighbors=allNeighbors, 
      neighbors=neighbors, 
      diagonals=diagonals, 
      maze=maze}