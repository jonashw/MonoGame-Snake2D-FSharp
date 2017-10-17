module Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
open PixelPerfectRenderer

type Game1 () as x =
    inherit Game()
    do x.Content.RootDirectory <- "Content"
    do x.Window.Title <- "Snake!"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable pixelTexture: Texture2D = null
    let mutable pixelRenderer = Unchecked.defaultof<PixelPerfectRenderer>
    let mutable snake = 
        Snake.makeSnake
            (60,5)
            (25,5)
            (X, Positive)
    let mutable bouncyBlocks =
        [
            BouncyBlock.make (30,20) (31,20)
            BouncyBlock.make (40,34) (40,33)
            BouncyBlock.make (50,20) (52,21)
            BouncyBlock.make (30,20) (28,21)
            BouncyBlock.make (50,15) (51,17)
            BouncyBlock.make (20,15) (21,13)
        ]
    let blocks = 
        let makePerimeter (x1,y1) (x2,y2): Tile.Tile list = 
            List.concat
                [
                    [y1..y2] |> List.collect (fun y -> [x1,y; x2,y])
                    [x1..x2] |> List.collect (fun x -> [x,y1; x,y2])
                ]
        List.concat 
            [
                makePerimeter ( 0, 0) (79,44) //level border
                makePerimeter (20,10) (59,35) //inner cloister
            ]
    let obstacleTiles = blocks |> Set.ofList
    let staticObstacleIn t = obstacleTiles |> Set.contains t
    let blockRectangles = 
        blocks |> List.map Tile.toRect
    //do printfn "%A" blockRectangles
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
            (70,43)
            (Reflect)
        ]
    let mutable nextHeading = None
    let backgroundColor = Blank, Lowest
 
    override x.Initialize() =
        (* Let it be known that from now on,
        ** this game's display will adhere to a 16:9 aspect ratio in landscape orientation 
        ** and a 9:16 aspect ratio in portrait orientation.
        ** All levels will be designed with this in mind.
        ** Furthermore, all levels will be playable in either landscape or portrait orientation.
        ** A transition from one orientation to the other mid-game should not affect gameplay in any way;
        ** the player will simply have a new perspective on the current level. 
        ** For now, let's use a fixed level size of 80 tiles x 45 tiles *)
        let aspectRatio = 16.0f / 9.0f
        let getScreenResolutionByWidth width = 
            let height = (float32 width) / aspectRatio |> int
            {Width = width; Height = height}
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
        pixelRenderer <- new PixelPerfectRenderer(
            graphics,
            (getScreenResolutionByWidth 800), 
            (getScreenResolutionByWidth 2000))
        base.Initialize()
 
    override x.LoadContent() =
        do pixelTexture <- new Texture2D(graphics.GraphicsDevice, 1, 1, false, SurfaceFormat.Color)
        pixelTexture.SetData([| Color.White |])
 
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
        bouncyBlocks <- bouncyBlocks |> List.map (BouncyBlock.update gameTime.ElapsedGameTime staticObstacleIn)
 
    override x.Draw (gameTime) =
        pixelRenderer.Begin()
        do x.GraphicsDevice.Clear <| toColor backgroundColor
        spriteBatch.Begin()
        do Snake.draw spriteBatch pixelTexture snake
        for wh in wormholes do
            Wormhole.draw spriteBatch pixelTexture wh
        for b in blocks do
            Block.draw spriteBatch pixelTexture b
        for b in bouncyBlocks do
            BouncyBlock.draw spriteBatch pixelTexture b
        spriteBatch.End()
        pixelRenderer.End()