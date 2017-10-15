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
            }

let isTileDigital snake = 
    match snake.Segments with
    | [] -> true
    | (head,_) :: _ -> 
        ((int head.X) % Tile.size) = 0 &&
        ((int head.Y) % Tile.size) = 0

let makeSnake h t H =
    { Segments = [h,t]
    ; Heading = H
    ; GrowthLengthLeft = 0
    }

let makeBrokenSnake () =
    { Segments = [ new Vector2(500.0f, 50.0f), new Vector2(450.0f, 50.0f)
                 ; new Vector2(100.0f, 200.0f), new Vector2(100.0f, 400.0f)
                 ; new Vector2(450.0f, 300.0f), new Vector2(450.0f, 500.0f)]
    ; Heading = (X, Positive)
    ; GrowthLengthLeft = 0
    }

let private powerUpGrowthLength = function
| Regular -> 5
| Big -> 10
| Jumbo -> 20

let update 
    (snake: Snake) 
    (newHeading: Heading option) 
    (newPowerUp: PowerUp option)
    (headTransport: Vector2 option)
    (gameTimeElapse: TimeSpan): Snake =
    let headingUnitVector = headingToUnitVector snake.Heading 
    let validNewHeading = 
        newHeading
        |> Option.bind (fun h -> 
            let newUv = headingToUnitVector h
            (* The heading corresponding to a 90-degree turn (a valid turn)
            ** will have a unit vector that is *orthogonal* to the current heading's unit vector.
            ** We can determine orthogonality with Vector2.Dot *)
            if Vector2.Dot(headingUnitVector, newUv) = 0.0f 
            then Some h
            else None)
    let newGrowth = newPowerUp |> Option.map powerUpGrowthLength |> Option.defaultValue 0
    let growthLengthLeft = snake.GrowthLengthLeft + newGrowth
    match snake.Segments, validNewHeading with
    | [], _ -> 
        (* This is an "invalid" snake!
        ** The safest thing to do is to recognize its possibility and pass the buck. *)
        snake
    | (head,_) :: _ as segments, Some h -> 
        (* The head departs from its former location with a new heading.
        ** The old segment crystallizes, and we start a new segment.
        ** Initially, the 2 vertices of the segment have the same position.
        ** Of course, as time passes, the leading vertex advances.  *) 
        { Segments = (head,head) :: segments
        ; Heading = h
        ; GrowthLengthLeft = snake.GrowthLengthLeft
        }
    | [(head,tail)], None ->
        (* The snake is composed of a single segment (a straight line).
        ** Advance the head and tail with the same heading, when appropriate. *)
        let newHead = head + headingUnitVector
        let newTail, newGrowthLengthLeft = 
            if snake.GrowthLengthLeft > 0
            then tail, growthLengthLeft - 1
            else tail + headingUnitVector, 0
        { Segments = [(newHead,newTail)] 
        ; Heading = snake.Heading
        ; GrowthLengthLeft = newGrowthLengthLeft
        }
    | (head, headMate) :: laterSegments, None ->
        (* The head continues on its current heading.
        ** The tail may advance, shortening the last segment.
        ** Finally, the last segment may shrink to zero-length and be elimited. *)
        let middleSegments = laterSegments |> ListOperations.dropLast
        let (tailPrior,tail) = laterSegments |> Seq.last
        let newFirstSegment = (head + headingUnitVector, headMate)
        if tail = tailPrior 
        then (* In this case, the last segment ceases to be relevant and is simply removed. *)
            { Segments = newFirstSegment :: middleSegments 
            ; Heading = snake.Heading
            ; GrowthLengthLeft = growthLengthLeft
            }
        elif snake.GrowthLengthLeft > 0
        then (* In this case, the snake is growing via its tail, so all that changes is the head. *)
            { Segments = newFirstSegment :: laterSegments
            ; Heading = snake.Heading
            ; GrowthLengthLeft = growthLengthLeft - 1
            }
        else
            (* In this case, we have normal tail shrinkage (no growth).
            ** The tail is paired with some vertex *other than* the head, 
            ** so may need to advance in a different heading.
            ** We can derive this heading with some help from the tail's mate. *)
            let tailAdvancementUnitVector = tailPrior - tail
            tailAdvancementUnitVector.Normalize()
            let newTail = tail + tailAdvancementUnitVector
            { Segments = newFirstSegment :: middleSegments @ [(tailPrior, newTail)]
            ; Heading = snake.Heading
            ; GrowthLengthLeft = 0
            }

let normalColor = Primary, Low
let growingColor = SecondaryB, High
let draw (sb: SpriteBatch) (t: Texture2D) (snake: Snake): unit =
    let color = Color.Lerp(toColor normalColor, toColor growingColor, (float32 snake.GrowthLengthLeft) / 40.0f)
    for (a,b) in snake.Segments do
        let w = int <| Math.Abs(a.X - b.X)
        let h = int <| Math.Abs(a.Y - b.Y)
        let x = int <| Math.Min(a.X, b.X)
        let y = int <| Math.Min(a.Y, b.Y)
        let r = new Rectangle(x, y, w + Tile.size, h + Tile.size)
        sb.Draw(t, r, color) 
    ()