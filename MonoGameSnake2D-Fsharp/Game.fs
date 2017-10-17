module Game
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
open PixelPerfectRenderer
open Level

type Game1 (level: Level) as x =
    inherit Game()
    do x.Content.RootDirectory <- "Content"
    do x.Window.Title <- "Snake!"
    let graphics = new GraphicsDeviceManager(x)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable pixelTexture: Texture2D = null
    let mutable pixelRenderer = Unchecked.defaultof<PixelPerfectRenderer>
    let mutable snake = level.Snake
    let mutable bouncyBlocks = level.BouncyBlocks
    let blocks = level.Blocks
    let staticObstacleIn: Tile.Tile -> bool = 
        let obstacleTiles = blocks |> Set.ofList
        fun t -> obstacleTiles |> Set.contains t
    let wormholes = level.Wormholes
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
        ** Each level has the ability to specify its own size. *)
        pixelRenderer <- 
            let levelWidthInPixels = level.WidthInTiles * Tile.size
            let getScreenResolutionByWidth width = 
                let aspectRatio = 16.0f / 9.0f
                let height = (float32 width) / aspectRatio |> int
                {Width = width; Height = height}
            new PixelPerfectRenderer(
                graphics,
                (getScreenResolutionByWidth levelWidthInPixels), 
                (getScreenResolutionByWidth graphics.GraphicsDevice.Adapter.CurrentDisplayMode.Width))
        spriteBatch <- new SpriteBatch(graphics.GraphicsDevice)
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