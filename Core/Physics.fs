module Physics
open System
open Microsoft.Xna.Framework
open Movement

type SimpleProjectile = 
    { Position: Vector2
    ; Velocity: Vector2
    ; Size: int }
    with
    member p.Rectangle = 
        Rectangle(
            int p.Position.X,
            int p.Position.Y,
            p.Size,
            p.Size)

let private stepBouncyByAxis
    (staticObstacleIn: Tile.Tile -> bool)
    (elapsedGameTime: TimeSpan) 
    (axisOfMovement: Axis)
    (p: SimpleProjectile): SimpleProjectile =
        let desiredMovement = 
            let roundingError = 
                (* A rounding error occurs when we create the intermediary Rectangle instance,
                ** which uses integers rather than floats. *)
                let original = p.Position.Component axisOfMovement
                let floor = Math.Floor(original |> float) |> float32
                Math.Abs(original - floor)
            Math.Abs(p.Velocity.Component axisOfMovement) + roundingError
        let blockRect = Rectangle(int p.Position.X, int p.Position.Y, p.Size, p.Size)
        let heading = axisOfMovement, (p.Velocity.ComponentDirection axisOfMovement)
        let movementAmount, nextVelocity = 
            Tile.tilesIntersectedBy blockRect  
            |> Seq.map (Tile.add heading 1)
            |> Seq.filter staticObstacleIn
            |> Seq.sortBy (fun t -> Vector2.DistanceSquared(p.Position, t |> Tile.toVector2))
            |> Seq.tryHead
            |> Option.map Tile.toRect
            |> Option.map (fun obstacle -> Math.Abs(blockRect.Edge heading - obstacle.Edge (reflect heading)) |> float32)
            |> Option.filter (fun allowedMovement -> allowedMovement <= desiredMovement)
            |> Option.map (fun movement -> 
                   let nextVelocity = p.Velocity.Reflect axisOfMovement 
                   printfn "A SimpleProjectile collided (bouncy) with an obstacle. Velocity changed from %A to %A" p.Velocity nextVelocity
                   movement, nextVelocity)
            |> Option.defaultValue (desiredMovement, p.Velocity)
        { Position = p.Position.AddComponent movementAmount heading 
        ; Velocity = nextVelocity 
        ; Size = p.Size }

let updateBouncy
    (staticObstacleIn: Tile.Tile -> bool) 
    (elapsedGameTime: TimeSpan) 
    (p: SimpleProjectile): SimpleProjectile =
        // As it turns out, stepping each axis individually is far simpler than stepping them together.
        p |> stepBouncyByAxis staticObstacleIn elapsedGameTime X
          |> stepBouncyByAxis staticObstacleIn elapsedGameTime Y 