module Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
 
type Game1 () as x =
    inherit Game()
 
    do x.Content.RootDirectory <- "Content"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable snakeTexture: Texture2D = null
    let mutable snake = 
        Snake.makeSnake
            (Vector2(100.0f, 100.0f))
            (Vector2(0.0f, 100.0f))
            (X, Positive)
    let mutable nextHeading = None
    let mutable vp: Viewport = Unchecked.defaultof<Viewport>
    let backgroundColor = Blank, Lowest
 
    override x.Initialize() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        vp <- graphics.GraphicsDevice.Viewport
        base.Initialize()
        ()
 
    override x.LoadContent() =
        do snakeTexture <- new Texture2D(graphics.GraphicsDevice, 1, 1, false, SurfaceFormat.Color)
        snakeTexture.SetData([| Color.White |])
        ()
 
    override x.Update (gameTime) =
        let kb = Keyboard.GetState()
        if(kb.IsKeyDown(Keys.Escape)) then x.Exit()
        let newHeading =
            if   kb.IsKeyDown(Keys.Up)    || kb.IsKeyDown(Keys.K) then Some(Y,Negative)
            elif kb.IsKeyDown(Keys.Down)  || kb.IsKeyDown(Keys.J) then Some(Y,Positive)
            elif kb.IsKeyDown(Keys.Left)  || kb.IsKeyDown(Keys.H) then Some(X,Negative)
            elif kb.IsKeyDown(Keys.Right) || kb.IsKeyDown(Keys.L) then Some(X,Positive)
            else None
        if newHeading |> Option.isSome then nextHeading <- newHeading
        let newPowerUp =
            if   kb.IsKeyDown(Keys.D1) then Some(PowerUp.Regular)
            elif kb.IsKeyDown(Keys.D2) then Some(PowerUp.Big)
            elif kb.IsKeyDown(Keys.D3) then Some(PowerUp.Jumbo)
            else None
        let nextGridOkHeading =
            nextHeading
            |> Option.filter (fun _ -> Snake.isTileDigital snake)
        do snake <- Snake.update snake nextGridOkHeading newPowerUp None (gameTime.ElapsedGameTime) 
 
    override x.Draw (gameTime) =
        do x.GraphicsDevice.Clear <| toColor backgroundColor
        spriteBatch.Begin()
        do Snake.draw spriteBatch snakeTexture snake
        spriteBatch.End()
        ()