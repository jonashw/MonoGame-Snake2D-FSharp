module Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open Snake
open PalleteColor
 
type Game1 () as x =
    inherit Game()
 
    do x.Content.RootDirectory <- "Content"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable snakeTexture: Texture2D = null
    let mutable snake = 
        { Head = Vector2(100.0f, 100.0f)
        ; Tail = Vector2(0.0f, 100.0f)
        ; Body = []
        ; Heading = X, Positive
        ; GrowthLengthLeft = 0
        }
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
            if   kb.IsKeyDown(Keys.Up)    then Some(Y,Negative)
            elif kb.IsKeyDown(Keys.Down)  then Some(Y,Positive)
            elif kb.IsKeyDown(Keys.Left)  then Some(X,Negative)
            elif kb.IsKeyDown(Keys.Right) then Some(X,Positive)
            else None
        if newHeading |> Option.isSome then nextHeading <- newHeading
        let newPowerUp =
            if   kb.IsKeyDown(Keys.D1) then Some(PowerUp.Regular)
            elif kb.IsKeyDown(Keys.D2) then Some(PowerUp.Big)
            elif kb.IsKeyDown(Keys.D3) then Some(PowerUp.Jumbo)
            else None
        let nextGridOkHeading =
            nextHeading
            |> Option.filter (fun _ -> ((int snake.Head.X) % Tile.size) = 0 && ((int snake.Head.Y) % Tile.size) = 0)
        do snake <- Snake.update snake nextGridOkHeading newPowerUp None (gameTime.ElapsedGameTime) 
        //printfn "%A" <| snake.Head :: snake.Body @ [snake.Tail]
        printfn "%A %A" (snake.Head) (vp.X + vp.Width, vp.Y + vp.Height)
        let nextTransportLocation =
            let w = (vp.X + vp.Width)  |> float32
            let h = (vp.Y + vp.Height) |> float32
            if   snake.Head.X > w
            then Some <| Vector2(0.0f, snake.Head.Y)
            elif snake.Head.X < 0.0f 
            then Some <| Vector2(w, snake.Head.Y)
            elif snake.Head.Y > h
            then Some <| Vector2(snake.Head.X, 0.0f)
            elif snake.Head.Y < 0.0f
            then Some <| Vector2(snake.Head.X, h)
            else None
        match nextTransportLocation with
        | None -> ()
        | Some l -> printfn "next transport location = %A" l
        ()

 
    override x.Draw (gameTime) =
        do x.GraphicsDevice.Clear <| toColor backgroundColor
        spriteBatch.Begin()
        do Snake.draw spriteBatch snakeTexture snake
        spriteBatch.End()
        ()