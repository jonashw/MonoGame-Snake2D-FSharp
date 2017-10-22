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

let private getLeadRectAtPosition (head: Vector2) H lengthInTiles =
    //The head position given here could be interpreted as either Min or Max, depending on the Heading.
    (*                         .-.         H = heading
        H=X+     H=X-          | |   H=Y+  h = head position
                               '-M         m = min
       .----. h m----.         h           M = MAX
       '----M   '----'         m-.         
                               | |   H=Y-
                               '-' 
    *)
    let l = Tile.size * lengthInTiles |> float32
    let s = Tile.sizeF32
    let lx = Vector2(l, 0.0f)
    let ly = Vector2(0.0f, l)
    let h = Vector2(0.0f, s)
    let w = Vector2(s, 0.0f)
    match H with
    | X, Negative -> RectangleF.fromMinAndMax head (head + lx + h)
    | Y, Negative -> RectangleF.fromMinAndMax head (head + ly + w)
    | X, Positive -> RectangleF.fromMinAndMax (head - lx) (head + h) 
    | Y, Positive -> RectangleF.fromMinAndMax (head - ly) (head + w) 

let getLeadRectAfterRect (r: Rectangle) (heading: Heading): RectangleF =
    let w = Vector2(r.Height |> float32, 0.0f)
    let h = Vector2(0.0f, r.Height |> float32)
    match heading with
    | X, Negative -> RectangleF.fromMinAndMax r.Min (r.Min + h)
    | Y, Negative -> RectangleF.fromMinAndMax r.Min (r.Min + w)
    | X, Positive -> RectangleF.fromMinAndMax (r.Max - h) r.Max
    | Y, Positive -> RectangleF.fromMinAndMax (r.Max - w) r.Max

let makeSnake (head: Tile.Tile) (H:Heading) (lengthInTiles: int) (s: float32) =
    { LeadSegment = { Rect = getLeadRectAtPosition (head |> Tile.toVector2) H lengthInTiles ; Heading = H }
    ; FollowSegments = []
    ; GrowthLengthLeft = 0
    ; SpeedFactor = s }

let shrinkRect (r: RectangleF) (delta: Vector2) (heading: Heading): RectangleF =
    let { Min=m ; Max=M } = r
    match heading with
    | _, Negative -> RectangleF.fromMinAndMax m (M + delta)
    | _, Positive -> RectangleF.fromMinAndMax (m + delta) M

let growRect (r: RectangleF) (delta: Vector2) (heading: Heading): RectangleF =
    let { Min=m ; Max=M } = r
    match heading with
    | _, Negative -> RectangleF.fromMinAndMax (m + delta) M
    | _, Positive -> RectangleF.fromMinAndMax m (M + delta)

let getNextSegment ({ Rect = prevRect ; Heading = prevHeading } as prev: SnakeSegment) (nextHeading: Heading): SnakeSegment =
    let s = Vector2(Tile.sizeF32, Tile.sizeF32)
    let m, M = prevRect.Min, prevRect.Max
    let next m M = { Rect = RectangleF.fromMinAndMax m M ; Heading = nextHeading }
    match prevHeading, nextHeading with
    | (X, Negative), (Y, _)
    | (Y, Negative), (X, _) -> next m (m + s)
    | (X, Positive), (Y, _) 
    | (Y, Positive), (X, _) -> next (M - s) M
    | _ -> prev

let private updatePowerup (powerup: PowerUp option) (snake: Snake) : Snake = 
    match powerup with
    | None -> snake
    | Some pu ->
        let boost = match pu with Regular -> 5 | Big -> 10 | Jumbo -> 20
        { snake with GrowthLengthLeft = snake.GrowthLengthLeft + boost }

let private updateTurn (turnHeadingOption: Heading option) (snake: Snake): Snake = 
    turnHeadingOption
    |> Option.filter (orthogonal snake.LeadSegment.Heading)
    |> function
       | None -> snake
       | Some turnHeading -> 
           (* The head departs from its former location with a new heading.
           ** The old segment crystallizes, and we start a new segment. *)
           let correctedLeadSegment = { snake.LeadSegment with Rect = Tile.fitRect snake.LeadSegment.Rect }
           { snake with LeadSegment = turnHeading |> getNextSegment correctedLeadSegment 
                      ; FollowSegments = correctedLeadSegment :: snake.FollowSegments }

let private updateTeleport (t: Teleport option) (snake: Snake): Snake = 
    match t with
    | None -> snake
    | Some teleport ->
        (* The head is about to teleport!
        ** Note that the heading can change during teleport. *)
        let nextHeading = snake.LeadSegment.Heading |> transformHeading (teleport.HeadingTransform)
        // To prevent an infinite loop, we have to make sure the snake exits the wormhole completely.
        let newLeadSegment = { Rect = getLeadRectAfterRect teleport.To nextHeading ; Heading = nextHeading }
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
        |> updateTeleport teleport
        |> updatePowerup powerup
        |> updateHead elapsed
        |> updateTailAndGrowth elapsed
        |> updateTurn turn

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
    if debug then do
        snake.LeadSegment |> drawSegmentDebug
        snake.LeadSegment |> drawSegmentDebugDots
        snake.FollowSegments |> List.iter drawSegmentDebug
        snake.FollowSegments |> List.iter drawSegmentDebugDots

let headRectangleF (s: Snake) =
    s.LeadSegment.Rect