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
    let mutable pixelTexture: Texture2D = null
    let mutable snake = 
        Snake.makeSnake
            (60,5)
            (25,5)
            (X, Positive)
    let wormholes = 
        [ Wormhole.makeWormhole 
            (Complement,Normal) 
            (10,5) 
            (70,40)
            (Rotate Clockwise)
        ; Wormhole.makeWormhole 
            (SecondaryB,Normal) 
            (10,40) 
            (70,5)
            (Noop)
        ; Wormhole.makeWormhole
            (SecondaryA,Lowest)
            (70,25)
            (70,45)
            (Reflect)
        ]
    let mutable nextHeading = None
    let mutable vp: Viewport = Unchecked.defaultof<Viewport>
    let backgroundColor = Blank, Lowest
 
    override x.Initialize() =
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        vp <- graphics.GraphicsDevice.Viewport
        base.Initialize()
        ()
 
    override x.LoadContent() =
        do pixelTexture <- new Texture2D(graphics.GraphicsDevice, 1, 1, false, SurfaceFormat.Color)
        pixelTexture.SetData([| Color.White |])
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
        let teleport = wormholes |> List.tryPick (Wormhole.tryTeleport <| Snake.headPosition snake)
        printfn "teleport: %A" teleport
        do snake <- Snake.update snake nextGridOkHeading newPowerUp teleport (gameTime.ElapsedGameTime) 
 
    override x.Draw (gameTime) =
        do x.GraphicsDevice.Clear <| toColor backgroundColor
        spriteBatch.Begin()
        do Snake.draw spriteBatch pixelTexture snake
        for wh in wormholes do
            Wormhole.draw spriteBatch pixelTexture wh
        spriteBatch.End()
        ()