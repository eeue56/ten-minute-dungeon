module Input (Direction(Up, Down, Left, Right, None), Input, playerDirection) where

import Keyboard
import Signal exposing (merge, (<~), (~))

type Direction = Up | Down | Left | Right | None 

type alias Input = { 
    direction: Direction
}

facing ds = if 
  | ds == {x=0,y=1} -> Up
  | ds == {x=0,y=-1} -> Down
  | ds == {x=1,y=0} -> Right
  | ds == {x=-1,y=0} -> Left
  | otherwise -> None

playerDirection : Signal Direction 
playerDirection = merge (facing <~ Keyboard.arrows) (facing <~ Keyboard.wasd)
