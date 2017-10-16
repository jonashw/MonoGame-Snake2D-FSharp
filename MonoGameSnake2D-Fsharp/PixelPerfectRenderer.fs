module PixelPerfectRenderer

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework

type ScreenResolution = { Width: int; Height: int }

(* Inspirations for this implementation:
**      - "Scaling entire screen in XNA" [https://stackoverflow.com/a/7603116/943730]
**      - "What is the best Screen Resolution for pixel art 2d game." [http://community.monogame.net/t/what-is-best-screen-resolution-for-pixel-art-2d-game/7876]
*)
type PixelPerfectRenderer
    ( graphics: GraphicsDeviceManager
    , nativeResolution: ScreenResolution
    , finalDisplayResolution: ScreenResolution) =
    let device = graphics.GraphicsDevice
    let renderTarget = new RenderTarget2D(device, nativeResolution.Width, nativeResolution.Height)
    let scaleBatch = new SpriteBatch(device)
    let finalDisplayViewport = new Rectangle(0, 0, finalDisplayResolution.Width, finalDisplayResolution.Height)
    do
        graphics.PreferredBackBufferWidth <- finalDisplayResolution.Width
        graphics.PreferredBackBufferHeight <- finalDisplayResolution.Height
        graphics.ApplyChanges()
    member x.Begin () =
        device.SetRenderTarget(renderTarget)
    member x.End () =
        device.SetRenderTarget(null)
        scaleBatch.Begin(
            SpriteSortMode.Deferred,
            BlendState.NonPremultiplied,
            SamplerState.PointClamp,
            DepthStencilState.Default,
            RasterizerState.CullNone)
        scaleBatch.Draw(
            renderTarget, 
            finalDisplayViewport,
            Color.White)
        scaleBatch.End()