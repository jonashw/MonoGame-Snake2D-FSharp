[<AutoOpen>]
module Draw

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework

type Microsoft.Xna.Framework.Graphics.SpriteBatch with
    member sb.drawOutline t lineWidth color (r: Rectangle): unit =
        sb.Draw(t, new Rectangle(r.X, r.Y, lineWidth, r.Height + lineWidth), color)
        sb.Draw(t, new Rectangle(r.X, r.Y, r.Width + lineWidth, lineWidth), color)
        sb.Draw(t, new Rectangle(r.X + r.Width, r.Y, lineWidth, r.Height + lineWidth), color)
        sb.Draw(t, new Rectangle(r.X, r.Y + r.Height, r.Width + lineWidth, lineWidth), color)