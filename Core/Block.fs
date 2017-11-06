module Block
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Block = Tile.Tile * BlockPermissibility
and BlockPermissibility = NotPermissive | SnakeOnly 

let private notPermissiveColor = Color(33,33,33)
let private snakeOnlyColor = Color(99,99,99)

let draw (sb:SpriteBatch) (t: Texture2D) (b: Block) =
    let color = match (snd b) with
                | NotPermissive -> notPermissiveColor
                | SnakeOnly -> snakeOnlyColor
    sb.Draw(t, Tile.toRect (fst b), color)