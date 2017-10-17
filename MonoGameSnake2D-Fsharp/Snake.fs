module Snake
open Microsoft.Xna.Framework
open System
open Microsoft.Xna.Framework.Graphics
open PalleteColor
open Movement
open PowerUp

type Snake = 
    private { Segments: (Vector2 * Vector2) list
            ; Heading: Heading
            ; GrowthLengthLeft: int 
            ; SpeedFactor: float32 }

let segmentToRectF (a: Vector2, b: Vector2): RectangleF =
    RectangleF.fromParts 
        (Math.Min(a.X, b.X))
        (Math.Min(a.Y, b.Y))
        (Tile.sizeF32 + Math.Abs(a.X - b.X))
        (Tile.sizeF32 + Math.Abs(a.Y - b.Y))

let isTileDigital snake = 
    match snake.Segments with
    | [] -> true
    | (head,_) :: _ -> 
        ((int head.X) % Tile.size) = 0 &&
        ((int head.Y) % Tile.size) = 0

let headPosition s =
    match s.Segments with
    | [] -> Vector2(-100.0f, -100.0f)
    | (head,_) :: _ -> head

let makeSnake h t H s =
    { Segments = [ h |> Tile.toVector2, t |> Tile.toVector2 ]
    ; Heading = H
    ; GrowthLengthLeft = 0
    ; SpeedFactor = s }

let private powerUpGrowthLength = function
| Regular -> 5
| Big -> 10
| Jumbo -> 20

let update 
    (snake: Snake) 
    (newHeading: Heading option) 
    (newPowerUp: PowerUp option)
    (headTeleport: Teleport option)
    (gameTimeElapse: TimeSpan): Snake =
    let positionDelta = Vector2.Multiply(headingToUnitVector snake.Heading, snake.SpeedFactor)
    let validNewHeading = 
        newHeading
        |> Option.bind (fun h -> 
            let newUv = headingToUnitVector h
            (* The heading corresponding to a 90-degree turn (a valid turn)
            ** will have a unit vector that is *orthogonal* to the current heading's unit vector.
            ** We can determine orthogonality with Vector2.Dot *)
            if Vector2.Dot(positionDelta, newUv) = 0.0f 
            then Some h
            else None)
    let newGrowth = newPowerUp |> Option.map powerUpGrowthLength |> Option.defaultValue 0
    let growthLengthLeft = snake.GrowthLengthLeft + newGrowth
    match snake.Segments, validNewHeading, headTeleport with
    | [], _, _ -> 
        (* This is an "invalid" snake!
        ** The safest thing to do is to recognize its possibility and pass the buck. *)
        snake
    | (head,_) :: _ as segments, Some h, _ -> 
        (* The head departs from its former location with a new heading.
        ** The old segment crystallizes, and we start a new segment.
        ** Initially, the 2 vertices of the segment have the same position.
        ** Of course, as time passes, the leading vertex advances.  *) 
        { Segments = (head,head) :: segments
        ; Heading = h
        ; GrowthLengthLeft = snake.GrowthLengthLeft
        ; SpeedFactor = snake.SpeedFactor }
    | [(head,tail)], None, None ->
        (* The snake is composed of a single segment (a straight line).
        ** Advance the head and tail with the same heading, when appropriate. *)
        let newHead = head + positionDelta
        let newTail, newGrowthLengthLeft = 
            if snake.GrowthLengthLeft > 0
            then tail, growthLengthLeft - 1
            else tail + positionDelta, 0
        { Segments = [(newHead,newTail)] 
        ; Heading = snake.Heading
        ; GrowthLengthLeft = newGrowthLengthLeft
        ; SpeedFactor = snake.SpeedFactor }
    | (head, headMate) :: laterSegments, None, Some teleport ->
        (* The head is about to teleport!
        ** For simplicity, let's ignore Growth/Shrinkage. 
        ** The heading could change during teleport in the future, but no change is possible for now. *)
        let nextHeading = snake.Heading |> transformHeading (teleport.HeadingTransform)
        // To prevent an infinite loop, we have to make sure the snake exits the wormhole completely.
        printfn "TELEPORT!"
        let teleportOffset = Vector2.Multiply(headingToUnitVector nextHeading, float32 Tile.size)
        let nextHead = teleport.To + teleportOffset
        { Segments = (nextHead, nextHead) :: (head, headMate) :: laterSegments
        ; Heading = nextHeading
        ; GrowthLengthLeft = growthLengthLeft 
        ; SpeedFactor = snake.SpeedFactor }
    | (head, headMate) :: laterSegments, None, None ->
        (* The head continues on its current heading.
        ** The tail may advance, shortening the last segment.
        ** Finally, the last segment may shrink to zero-length and be elimited. *)
        let middleSegments = laterSegments |> ListOperations.dropLast
        let (tailPrior,tail) = laterSegments |> Seq.last
        let newFirstSegment = (head + positionDelta, headMate)
        if tail = tailPrior 
        then (* In this case, the last segment ceases to be relevant and is simply removed. *)
            { Segments = newFirstSegment :: middleSegments 
            ; Heading = snake.Heading
            ; GrowthLengthLeft = growthLengthLeft
            ; SpeedFactor = snake.SpeedFactor }
        elif snake.GrowthLengthLeft > 0
        then (* In this case, the snake is growing via its tail, so all that changes is the head. *)
            { Segments = newFirstSegment :: laterSegments
            ; Heading = snake.Heading
            ; GrowthLengthLeft = growthLengthLeft - 1 
            ; SpeedFactor = snake.SpeedFactor }
        else
            (* In this case, we have normal tail shrinkage (no growth).
            ** The tail is paired with some vertex *other than* the head, 
            ** so may need to advance in a different heading.
            ** We can derive this heading with some help from the tail's mate. *)
            let tailPositionDelta = 
                let tailAdvancementUnitVector = tailPrior - tail
                tailAdvancementUnitVector.Normalize()
                Vector2.Multiply(tailAdvancementUnitVector, snake.SpeedFactor)
            let newTail = tail + tailPositionDelta
            { Segments = newFirstSegment :: middleSegments @ [(tailPrior, newTail)]
            ; Heading = snake.Heading
            ; GrowthLengthLeft = 0
            ; SpeedFactor = snake.SpeedFactor }

let segmentToRect (a: Vector2, b: Vector2): Rectangle =
    Rectangle(
        int <| Math.Min(a.X, b.X),
        int <| Math.Min(a.Y, b.Y),
        Tile.size + (int <| Math.Abs(a.X - b.X)),
        Tile.size + (int <| Math.Abs(a.Y - b.Y)))

let headRectangle s =
    match s.Segments with
    | [] -> Rectangle.Empty
    | s :: _ -> segmentToRect s

let headRectangleF s =
    match s.Segments with
    | [] -> RectangleF.empty
    | s :: _ -> segmentToRectF s

let normalColor = Primary, Low
let growingColor = SecondaryB, High
let draw (sb: SpriteBatch) (t: Texture2D) (snake: Snake): unit =
    let color = Color.Lerp(toColor normalColor, toColor growingColor, (float32 snake.GrowthLengthLeft) / 40.0f)
    snake.Segments 
    |> List.map segmentToRect
    |> List.iter (fun r -> sb.Draw(t, r, color))
    (*
    snake.Segments 
    |> List.map segmentToRect
    |> List.iter (sb.drawOutline t 1 Color.Black)
    snake.Segments 
    |> List.collect (fun (a,b) -> [a; b]) 
    |> List.iter (fun v -> sb.Draw(t, v, Color.Magenta))
    *)