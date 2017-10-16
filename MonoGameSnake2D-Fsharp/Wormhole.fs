module Wormhole
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open PalleteColor
open Movement

type Wormhole = 
    private { Color: PalleteColor
            ; EntranceA: Tile.Tile 
            ; EntranceB: Tile.Tile
            ; HeadingTransform: HeadingTransform
            } 

let makeWormhole c ea eb ht = { Color = c; EntranceA = ea; EntranceB = eb; HeadingTransform = ht }

let tryTeleport (projectile: Vector2) (wh: Wormhole): Teleport option =
    option {
        let entranceA = wh.EntranceA |> Tile.toVector2
        let entranceB = wh.EntranceB |> Tile.toVector2
        if entranceA = projectile 
        then return { From = projectile; To = entranceB; HeadingTransform = wh.HeadingTransform }
        else
        if entranceB = projectile
        then return { From = projectile; To = entranceA; HeadingTransform = wh.HeadingTransform  }
    }

let draw (sb: SpriteBatch) (t: Texture2D) (w: Wormhole): unit =
    let color = toColor w.Color
    let drawEntrance (e: Vector2) = 
        let rect = Rectangle(int e.X, int e.Y, Tile.size, Tile.size)
        sb.Draw(t, rect, color) 
    w.EntranceA |> Tile.toVector2 |> drawEntrance
    w.EntranceB |> Tile.toVector2 |> drawEntrance