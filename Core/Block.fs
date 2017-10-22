module Block
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Block = Tile.Tile

let private color = Color(33,33,33)

let draw (sb:SpriteBatch) (t: Texture2D) (b: Block) =
    sb.Draw(t, Tile.toRect b, color)