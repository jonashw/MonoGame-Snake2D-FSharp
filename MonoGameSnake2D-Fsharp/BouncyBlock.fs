module BouncyBlock
open Tile

open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework
open System
open Movement

let color = Color(200,200,200)

type BouncyBlock =
    private { Position: Vector2
            ; Velocity: Vector2
            ; Size: int
            }

let make (position: Tile) (aimedAt: Tile): BouncyBlock =
    let p = position |> toVector2 
    let t = aimedAt  |> toVector2 
    let v = t - p
    v.Normalize()
    { Position = p ; Velocity = v; Size = Tile.size }

let getRect b = 
    new Rectangle(int b.Position.X, int b.Position.Y, b.Size, b.Size)

let positionToRect (p: Vector2) (s:int): Rectangle = 
    new Rectangle(int p.X, int p.Y, s, s)

let draw (sb:SpriteBatch) (t: Texture2D) (b: BouncyBlock) =
    sb.Draw(t, getRect b, color)

let printDebug (b: BouncyBlock): unit =
    printfn "{Position: %A Velocity: %A Size: %A}" (b.Position) (b.Velocity) (b.Size)

let update (elapsedGameTime: TimeSpan) (staticObstacleIn: Tile -> bool) (b: BouncyBlock): BouncyBlock =
    let stepByAxis (axisOfMovement: Axis) (currentPosition: Vector2) (currentVelocity: Vector2): Vector2 * Vector2 =
        let rect = positionToRect currentPosition b.Size
        let direction = if vectorComponent currentVelocity axisOfMovement >= 0.0f
                        then Positive
                        else Negative
        //1. Get the coordinate of the forward-facing edge.
        let forwardEdge = Movement.forwardEdge rect (axisOfMovement, direction) 
        let forwardTileEdge = forwardEdge / Tile.size
        //2. Figure which lines of tiles the bounding box intersects with. This will give the min/max tile values on the OPPOSITE axis.
        //   For example, if we'ere walking left, perhaps the player intersects with the horizontal rows 32, 33, and 34.  
        //   That is, tiles with y = 32 * TS, y = 33 * TS, and y = 34 * TS, where TS = tile size.
        let otherAxis = Movement.otherAxis axisOfMovement
        let intersectedTiles = Tile.tilesIntersectedBy rect 
        let tileLineCriteria =
            intersectedTiles
            |> List.map (Tile.tileComponent otherAxis)
            |> Set.ofList
        //3. Scan along those lines of tiles and in the direction of movement until you find the closest static obstacle.
        //   Then loop through every moving obstacle as well, determining the closet obstacle (static OR moving)
        let nextNTiles n startingTile =
            [1..n] 
            |> List.map (Tile.add startingTile (axisOfMovement, direction))
        let nextClosestTiles = 
            intersectedTiles
            |> List.collect (nextNTiles 3)
            |> List.sortBy (fun t -> Vector2.DistanceSquared(b.Position, t |> Tile.toVector2))
        let nextClosestObstacleTile = nextClosestTiles |> List.tryFind staticObstacleIn
        //4. The total movement of the player along that direction is then the min between the distance to the closest obstacle and the amount you wanted to move in the first place.
        let desiredMovementAmount = System.Math.Abs(vectorComponent currentVelocity axisOfMovement)
        let movementAmount, nextVelocity = 
            match nextClosestObstacleTile with
            | None -> desiredMovementAmount, currentVelocity
            | Some(t) -> 
               let nextObstacleReverseEdge = 
                   let obstacleRect = t |> Tile.toRect 
                   let reverseDirection = direction |> Movement.reverse
                   Movement.forwardEdge obstacleRect (axisOfMovement, reverseDirection)
               let allowedMovementAmount = System.Math.Abs(forwardEdge - nextObstacleReverseEdge) |> float32
               if allowedMovementAmount < desiredMovementAmount
               then 
                   let nextVelocity = reflectVelocity axisOfMovement currentVelocity
                   printfn "BOUNCY BLOCK COLLISION! Velocity changed from %A to %A" currentVelocity nextVelocity
                   allowedMovementAmount, nextVelocity
               else
                   desiredMovementAmount, currentVelocity
        //5. Move player to then new position.  
        let nextPosition = Movement.vectorAddComponent currentPosition (axisOfMovement, direction) movementAmount
        nextPosition, nextVelocity
    let positionAfterX, velocityAfterX = stepByAxis X b.Position b.Velocity
    let positionAfterY, velocityAfterY = stepByAxis Y positionAfterX velocityAfterX
    { b with Position = positionAfterY ; Velocity = velocityAfterY }