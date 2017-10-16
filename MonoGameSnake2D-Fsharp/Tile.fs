module Tile

open Microsoft.Xna.Framework

let size = 10

type Tile = int * int 

let toVector2 (x,y) =
    Vector2(float32 <| size * x, float32 <| size * y)