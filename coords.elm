module Coords (Position, genBoard) where


type alias Position = {x: Int, y: Int}

genBoard : Int -> Int -> List (Int, Int)
genBoard m n =
    let 
        tuples x y = (x, y)
        columns : Int -> Int -> List (Int, Int)
        columns n x = List.map2 tuples [0..(n-1)] (List.repeat n x)
    in 
        List.concat <| List.map (columns n) [0..(m-1)]

