module Block
open Tile
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

let color = Color(33,33,33)

let draw (sb:SpriteBatch) (t: Texture2D) (b: Tile) =
    sb.Draw(t, toRect b, color)