module Wormhole
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open PalleteColor
open Movement

type Wormhole = 
    private { Color: PalleteColor
            ; Entrances: Tile.Tile * Tile.Tile
            ; HeadingTransform: HeadingTransform } 

let makeWormhole c ea eb ht: Wormhole = 
    { Color = c
    ; Entrances = ea, eb
    ; HeadingTransform = ht }

let tryTeleport (projectile: RectangleF) (wh: Wormhole): Teleport option =
    option {
        let entranceA = wh.Entrances |> fst |> Tile.toRect
        let entranceB = wh.Entrances |> snd |> Tile.toRect
        if projectile.Intersects entranceA
        then return { To = wh.Entrances |> snd |> Tile.toVector2 ; HeadingTransform = wh.HeadingTransform }
        else
        if projectile.Intersects entranceB
        then return { To = wh.Entrances |> fst |> Tile.toVector2 ; HeadingTransform = wh.HeadingTransform }
    }

let draw (sb: SpriteBatch) (t: Texture2D) (w: Wormhole): unit =
    let color = toColor w.Color
    let drawEntrance (e: Tile.Tile) = 
        let rect = Tile.toRect e
        sb.Draw(t, rect, color) 
    w.Entrances |> fst |> drawEntrance
    w.Entrances |> snd |> drawEntrance