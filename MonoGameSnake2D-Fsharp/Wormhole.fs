module Wormhole
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open PalleteColor
open Movement

type Wormhole = 
    private { Color: PalleteColor
            ; Entrances: Rectangle * Rectangle
            ; HeadingTransform: HeadingTransform } 

let makeWormhole c ea eb ht: Wormhole = 
    { Color = c
    ; Entrances = ea |> Tile.toRect, eb |> Tile.toRect
    ; HeadingTransform = ht }

let tryTeleport (projectile: RectangleF) (wh: Wormhole): Teleport option =
    let (ea, eb) = wh.Entrances 
    if projectile.Intersects ea
    then Some { To = eb ; HeadingTransform = wh.HeadingTransform }
    else
    if projectile.Intersects eb
    then Some { To = ea ; HeadingTransform = wh.HeadingTransform }
    else None

let draw (sb: SpriteBatch) (t: Texture2D) (w: Wormhole): unit =
    let color = toColor w.Color
    let drawEntrance (rect: Rectangle) = sb.Draw(t, rect, color) 
    w.Entrances |> fst |> drawEntrance
    w.Entrances |> snd |> drawEntrance