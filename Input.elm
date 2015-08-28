module Input (Action(MouseClick, Up, Down, Left, Right, None), Input, inputSignal) where

import Keyboard
import Mouse
import Signal exposing (merge, (<~), (~))

type Action = MouseClick Int Int | Up | Down | Left | Right | None 

type alias Input = { 
    action: Action
}

facing ds = if 
  | ds == {x=0,y=1} -> Up
  | ds == {x=0,y=-1} -> Down
  | ds == {x=1,y=0} -> Right
  | ds == {x=-1,y=0} -> Left
  | otherwise -> None

playerDirection : Signal Action 
playerDirection = merge (facing <~ Keyboard.arrows) (facing <~ Keyboard.wasd)

clickSignal = Signal.map2 (\isDown (x,y) -> if isDown then MouseClick x y else None) Mouse.isDown Mouse.position

inputSignal = merge clickSignal playerDirection