module BouncyBlock

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open System
open Movement
open Physics

type BouncyBlock = SimpleProjectile 

let make (position: Tile.Tile) (aimedAt: Tile.Tile): BouncyBlock =
    let p = position |> Tile.toVector2 
    let t = aimedAt  |> Tile.toVector2 
    let v = t - p
    v.Normalize()
    { Position = p ; Velocity = v ; Size = Tile.size}

let update (elapsedGameTime: TimeSpan) (staticObstacleIn: Tile.Tile -> bool) (b: BouncyBlock): BouncyBlock =
    updateBouncy staticObstacleIn elapsedGameTime b 

let printDebug (b: BouncyBlock): unit =
    printfn "{Position: %A Velocity: %A Size: %A}" (b.Position) (b.Velocity) (b.Size)

let private color = Color(200,200,200)

let draw (sb:SpriteBatch) (t: Texture2D) (b: BouncyBlock) =
    sb.Draw(t, b.Rectangle, color)