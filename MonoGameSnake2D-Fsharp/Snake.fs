module Snake
open Microsoft.Xna.Framework
open System
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
open PowerUp

type Snake = 
    private { LeadSegment: SnakeSegment
            ; FollowSegments: SnakeSegment list
            ; GrowthLengthLeft: int 
            ; SpeedFactor: float32 
            } 
and SnakeSegment = { Rect: RectangleF 
                   ; Heading: Heading 
                   } with member s.ForwardEdge = s.Rect.Edge s.Heading

let isTileDigital s = 
    ((s.LeadSegment.ForwardEdge |> int) % Tile.size) = 0

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
    { LeadSegment = { Rect = getLeadRect (head |> Tile.toVector2) H lengthInTiles ; Heading = H }
    ; FollowSegments = []
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

let getNextSegment ({ Rect = prevRect ; Heading = prevHeading } as prev: SnakeSegment) (nextHeading: Heading): SnakeSegment =
    let s = Vector2(Tile.sizeF32, Tile.sizeF32)
    let m, M = prevRect.Min, prevRect.Max
    let next m M = { Rect = RectangleF.fromMinAndMax m M ; Heading = nextHeading }
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
        (* The head departs from its former location with a new heading.
        ** The old segment crystallizes, and we start a new segment.
        ** Initially, the 2 vertices of the segment have the same position.
        ** Of course, as time passes, the leading vertex advances.  
        ** For simplicity, let's ignore tail Growth/Shrinkage during in this case. *)
        (* The heading corresponding to a 90-degree turn (a valid turn)
        ** will have a unit vector that is *orthogonal* to the current heading's unit vector.
        ** We can determine orthogonality with Vector2.Dot *)
        if Vector2.Dot(headingToUnitVector snake.LeadSegment.Heading, headingToUnitVector turnHeading) <> 0.0f 
        then snake
        else 
            { snake with LeadSegment = (getNextSegment snake.LeadSegment turnHeading) 
                       ; FollowSegments = snake.LeadSegment :: snake.FollowSegments }

let private updateTeleport (t: Teleport option) (snake: Snake): Snake = 
    match t with
    | None -> snake
    | Some teleport ->
        (* The head is about to teleport!
        ** Note that the heading can change during teleport.
        ** For simplicity, let's ignore Growth/Shrinkage during in this case. *)
        let nextHeading = snake.LeadSegment.Heading |> transformHeading (teleport.HeadingTransform)
        // To prevent an infinite loop, we have to make sure the snake exits the wormhole completely.
        let teleportOffset = Vector2.Multiply(headingToUnitVector nextHeading, float32 Tile.size)
        let newLeadSegment = { Rect = (getLeadRect (teleport.To + teleportOffset) nextHeading 0) ; Heading = nextHeading }
        printfn "TELEPORT! %A -> %A @ %A" snake.LeadSegment.Heading nextHeading newLeadSegment
        { LeadSegment = newLeadSegment 
        ; FollowSegments = snake.LeadSegment :: snake.FollowSegments
        ; GrowthLengthLeft = snake.GrowthLengthLeft 
        ; SpeedFactor = snake.SpeedFactor }

let private updateHead (elapsed: TimeSpan) (snake: Snake): Snake = 
    let positionDelta = Vector2.Multiply(headingToUnitVector snake.LeadSegment.Heading, snake.SpeedFactor)
    let newLeadSegment = { snake.LeadSegment with Rect = (growRect snake.LeadSegment.Rect positionDelta snake.LeadSegment.Heading) }
    { snake with LeadSegment = newLeadSegment }

let private updateTailAndGrowth (elapsed: TimeSpan) (snake: Snake): Snake = 
    let shrink lastSegment = 
        let tailPositionDelta = Vector2.Multiply(headingToUnitVector lastSegment.Heading, snake.SpeedFactor)
        { lastSegment with Rect = shrinkRect lastSegment.Rect tailPositionDelta lastSegment.Heading }
    let { Rect = lastSegmentRect ; Heading = lastSegmentHeading } as lastSegment = 
        snake.FollowSegments
        |> List.tryLast 
        |> Option.defaultValue (snake.LeadSegment)
    match snake.GrowthLengthLeft > 0, snake.FollowSegments with
    | true,  _  -> { snake with GrowthLengthLeft = snake.GrowthLengthLeft - 1}  
    | false, [] -> { snake with LeadSegment = shrink snake.LeadSegment }
    | false, _  -> 
        let followInit = snake.FollowSegments |> ListOperations.tryDropLast
        let newLast = snake.FollowSegments |> Seq.last |> shrink 
        if newLast.Rect.IsEmpty || newLast.Rect.IsInverted
        then { snake with FollowSegments = followInit }
        else { snake with FollowSegments = followInit @ [newLast] }

let update (snake: Snake) 
    (turn: Heading option) 
    (powerup: PowerUp option)
    (teleport: Teleport option)
    (elapsed: TimeSpan): Snake =
        snake
        |> updatePowerup powerup
        |> updateTeleport teleport
        |> updateTurn turn
        |> updateHead elapsed
        |> updateTailAndGrowth elapsed

let normalColor = Primary, Low
let growingColor = SecondaryB, High
let draw (sb: SpriteBatch) (t: Texture2D) (debug: bool) (snake: Snake): unit =
    let color = Color.Lerp(toColor normalColor, toColor growingColor, (float32 snake.GrowthLengthLeft) / 40.0f)
    let drawSegment s = sb.Draw(t, s.Rect.Round, color)
    let drawSegmentDebug s = sb.drawOutline t 1 Color.Black s.Rect.Round
    let drawSegmentDebugDots: SnakeSegment -> unit =
        (fun s -> [s.Rect.Min; s.Rect.Max])
        >> List.map (fun v -> Vector2(v.X |> int |> float32, v.Y |> int |> float32))
        >> List.iter (fun v -> sb.Draw(t, v, Color.Magenta))
    snake.LeadSegment |> drawSegment
    snake.FollowSegments |> List.iter drawSegment
    printfn "segment count = %A" (snake.FollowSegments.Length + 1)
    if debug then do
        snake.LeadSegment |> drawSegmentDebug
        snake.LeadSegment |> drawSegmentDebugDots
        snake.FollowSegments |> List.iter drawSegmentDebug
        snake.FollowSegments |> List.iter drawSegmentDebugDots

let headRectangleF (s: Snake) =
    s.LeadSegment.Rect