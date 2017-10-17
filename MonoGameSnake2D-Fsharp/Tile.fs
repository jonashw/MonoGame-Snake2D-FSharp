module Tile

open Microsoft.Xna.Framework

let size = 10
let sizeF = float size
let sizeF32 = float32 size

type Tile = int * int 

let toVector2 (x,y) =
    Vector2(float32 <| size * x, float32 <| size * y)

let toRect ((x,y): Tile) =
    Rectangle(x * size, y * size, size, size)

let toRectF ((x,y): Tile) =
    RectangleF.fromParts (x * size |> float32) (y * size |> float32) sizeF32 sizeF32

let fromVector2 (v: Vector2): Tile =
    let x = int <| v.X / sizeF32
    let y = int <| v.Y / sizeF32
    x, y

let tilesIntersectedBy (r: Rectangle): Tile list =
    let x0 = r.Left   / size
    let x1 = int <| System.Math.Ceiling((float r.Right) / sizeF)
    let y0 = r.Top    / size
    let y1 = int <| System.Math.Ceiling((float r.Bottom) / sizeF)
    seq {
        for x in [x0..x1-1] do
        for y in [y0..y1-1] do
        yield (x,y)
    } |> Seq.toList

let add (heading: Movement.Heading) (amount: int) ((x,y): Tile): Tile =
    match heading with
    | (Movement.X, Movement.Negative) -> x - amount, y
    | (Movement.X, Movement.Positive) -> x + amount, y
    | (Movement.Y, Movement.Negative) -> x, y - amount
    | (Movement.Y, Movement.Positive) -> x, y + amount