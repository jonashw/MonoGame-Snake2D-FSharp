module Snake
open Microsoft.Xna.Framework
open System
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
open PowerUp

type Snake = 
    private { Segments: SnakeSegment list
            ; GrowthLengthLeft: int 
            ; SpeedFactor: float32 }
and SnakeSegment = RectangleF * Heading

let isTileDigital snake = 
    match snake.Segments with
    | [] -> true
    | (r, h) :: _ -> ((r.Edge h |> int) % Tile.size) = 0

let private getLeadRect (head: Vector2) H lengthInTiles =
    //The head position given here could be interpreted as either Min or Max, depending on the Heading.
    (*                       .-.           H = heading
        H=X+                 | | H=Y+      h = head position
       .----.                '-M           m = min
       '----M h m----.         h           M = MAX
                '----'         m-.         
                 H=X-          | |   H=Y-
                               '-' 
    *)
    let h = head 
    let l = Tile.size * lengthInTiles |> float32
    let s = Tile.sizeF32
    match H with
    | X, Negative -> (* min *) RectangleF.fromMinAndMax h (Vector2(h.X + l, h.Y + s))
    | Y, Negative -> (* min *) RectangleF.fromMinAndMax h (Vector2(h.X + s, h.Y + l))
    | X, Positive -> (* MAX *) RectangleF.fromMinAndMax (Vector2(h.X - l, h.Y - s)) h 
    | Y, Positive -> (* MAX *) RectangleF.fromMinAndMax (Vector2(h.X - s, h.Y - l)) h 

let makeSnake (head: Tile.Tile) (H:Heading) (lengthInTiles: int) (s: float32) =
    { Segments = [ getLeadRect (head |> Tile.toVector2) H lengthInTiles, H ]
    ; GrowthLengthLeft = 0
    ; SpeedFactor = s }

let shrinkRect (r: RectangleF) (delta: Vector2) (heading: Heading): RectangleF =
    //printfn "shrinkRect(delta=%A ; heading=%A" delta heading
    let { Min=m ; Max=M } = r
    match heading with
    | _, Negative -> (* sub from MAX *) RectangleF.fromMinAndMax m (M+delta)
    | _, Positive -> (* sub from min *) RectangleF.fromMinAndMax (m+delta) M

let growRect (r: RectangleF) (delta: Vector2) (heading: Heading): RectangleF =
    let { Min=m ; Max=M } = r
    match heading with
    | _, Negative -> (* add to min *) RectangleF.fromMinAndMax (m+delta) M
    | _, Positive -> (* add to MAX *) RectangleF.fromMinAndMax m (M+delta)

let getNextSegment ((prevRect,prevHeading) as prev: SnakeSegment) (nextHeading: Heading): SnakeSegment =
    let s = Vector2(Tile.sizeF32, Tile.sizeF32)
    let m, M = prevRect.Min, prevRect.Max
    let next m M = RectangleF.fromMinAndMax m M, nextHeading
    match prevHeading, nextHeading with
    | (X, Negative), (Y, _)
    | (Y, Negative), (X, _) -> next m (m+s)
    | (X, Positive), (Y, _) 
    | (Y, Positive), (X, _) -> next (M-s) M
    | _ -> prev

let private updatePowerup (powerup: PowerUp option) (snake: Snake) : Snake = 
    match powerup with
    | None -> snake
    | Some pu ->
        let boost = match pu with Regular -> 5 | Big -> 10 | Jumbo -> 20
        { snake with GrowthLengthLeft = snake.GrowthLengthLeft + boost }

let private updateTurn (h: Heading option) (snake: Snake): Snake = 
    match h with
    | None -> snake
    | Some turnHeading -> 
        match snake.Segments with
        |  [] -> 
            (* This is an "invalid" snake!
            ** The safest thing to do is to recognize its possibility and pass the buck. *)
            snake
        | (_, leadHeading) as leadSegment :: _ ->
            (* The head departs from its former location with a new heading.
            ** The old segment crystallizes, and we start a new segment.
            ** Initially, the 2 vertices of the segment have the same position.
            ** Of course, as time passes, the leading vertex advances.  
            ** For simplicity, let's ignore tail Growth/Shrinkage during in this case. *)
            (* The heading corresponding to a 90-degree turn (a valid turn)
            ** will have a unit vector that is *orthogonal* to the current heading's unit vector.
            ** We can determine orthogonality with Vector2.Dot *)
            if Vector2.Dot(headingToUnitVector leadHeading, headingToUnitVector turnHeading) <> 0.0f 
            then snake
            else 
                let updatedSegments = (getNextSegment leadSegment turnHeading) :: snake.Segments
                { snake with Segments = updatedSegments }

let private updateTeleport (t: Teleport option) (snake: Snake): Snake = 
    match t with
    | None -> snake
    | Some teleport ->
        match snake.Segments with
        |  [] -> 
            (* This is an "invalid" snake!
            ** The safest thing to do is to recognize its possibility and pass the buck. *)
            snake
        | (_, leadHeading) :: _ ->
            (* The head is about to teleport!
            ** Note that the heading can change during teleport.
            ** For simplicity, let's ignore Growth/Shrinkage during in this case. *)
            let nextHeading = leadHeading |> transformHeading (teleport.HeadingTransform)
            // To prevent an infinite loop, we have to make sure the snake exits the wormhole completely.
            let teleportOffset = Vector2.Multiply(headingToUnitVector nextHeading, float32 Tile.size)
            let newLeadSegment = (getLeadRect (teleport.To + teleportOffset) nextHeading 0), nextHeading
            printfn "TELEPORT! %A -> %A @ %A" leadHeading nextHeading newLeadSegment
            { Segments = newLeadSegment :: snake.Segments
            ; GrowthLengthLeft = snake.GrowthLengthLeft 
            ; SpeedFactor = snake.SpeedFactor }

let private updateTimePassing (elapsed: TimeSpan) (snake: Snake): Snake = 
    match snake.Segments with
    |  [] -> 
        (* This is an "invalid" snake!
        ** The safest thing to do is to recognize its possibility and pass the buck. *)
        snake
    | (leadRect, leadHeading) as leadSegment :: followSegments ->
        (* The head continues on its current heading.
        ** The tail may advance, shortening the last segment.
        ** Finally, the last segment may shrink to zero-length and be elimited. *)
        let (lastSegmentRect,lastSegmentHeading) as lastSegment = Seq.last snake.Segments
        let positionDelta = Vector2.Multiply(headingToUnitVector leadHeading, snake.SpeedFactor)
        let newFirstSegment = (growRect leadRect positionDelta leadHeading), leadHeading
        match lastSegmentRect.IsEmpty || lastSegmentRect.IsInverted, snake.GrowthLengthLeft > 0 with
        | true, _ -> 
            printfn "EMPTY!"
            (* In this case, the last segment ceases to be relevant and is simply removed. *)
            { Segments = newFirstSegment :: (followSegments |> ListOperations.tryDropLast)
            ; GrowthLengthLeft = 0
            ; SpeedFactor = snake.SpeedFactor }
        | _, true -> 
            (* In this case, the snake is growing via its tail, so all that changes is the head. *)
            { Segments = newFirstSegment :: followSegments
            ; GrowthLengthLeft = snake.GrowthLengthLeft - 1 
            ; SpeedFactor = snake.SpeedFactor }
        | false, _ ->
            (* In this case, we have normal tail shrinkage (no growth). *)
            let tailPositionDelta = Vector2.Multiply(headingToUnitVector lastSegmentHeading, snake.SpeedFactor)
            let newTailSegment = (shrinkRect lastSegmentRect tailPositionDelta lastSegmentHeading), lastSegmentHeading
            //printfn "shrink the tail by %A. %A -> %A" tailPositionDelta lastSegment newTailSegment
            if leadSegment = lastSegment
            then 
                { Segments = [newFirstSegment ; newTailSegment]
                ; GrowthLengthLeft = 0
                ; SpeedFactor = snake.SpeedFactor }
            else 
                { Segments = newFirstSegment :: (followSegments |> ListOperations.tryDropLast) @ [newTailSegment]
                ; GrowthLengthLeft = 0
                ; SpeedFactor = snake.SpeedFactor }

let update (snake: Snake) 
    (turn: Heading option) 
    (powerup: PowerUp option)
    (teleport: Teleport option)
    (elapsed: TimeSpan): Snake =
        snake
        |> updatePowerup powerup
        |> updateTeleport teleport
        |> updateTurn turn
        |> updateTimePassing elapsed

let normalColor = Primary, Low
let growingColor = SecondaryB, High
let draw (sb: SpriteBatch) (t: Texture2D) (debug: bool) (snake: Snake): unit =
    let color = Color.Lerp(toColor normalColor, toColor growingColor, (float32 snake.GrowthLengthLeft) / 40.0f)
    snake.Segments 
    |> List.map (fun (r,_) -> r.Round)
    |> List.iter (fun r -> sb.Draw(t, r, color))
    printfn "segment count = %A" snake.Segments.Length
    if debug then do
        snake.Segments 
        |> List.map (fun s -> (fst s).Round)
        |> List.iter (sb.drawOutline t 1 Color.Black)
        snake.Segments 
        |> List.map fst
        |> List.collect (fun r -> [r.Min; r.Max])
        |> List.map (fun v -> Vector2(v.X |> int |> float32, v.Y |> int |> float32))
        |> List.iter (fun v -> sb.Draw(t, v, Color.Magenta))

let headRectangleF s =
    match s.Segments with
    | [] -> RectangleF.empty
    | (r,_) :: _ -> r