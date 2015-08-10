module Coords (Position, genBoard) where
    
type alias Position = {x: Float, y: Float}

genBoard x y = if
  | x < 0 || y < 0 -> []
  | x == 0 -> List.map (\y -> (0, y)) [1..y]
  | y == 0 -> List.map (\x -> (x, 0)) [1..x]
  | otherwise -> 
    (genBoard x 0) ++ 
      (genBoard 0 y) ++ 
        List.map2 (\x y -> (x, y)) [0..x] [0..y]