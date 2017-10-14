module Snake
open Microsoft.Xna.Framework
open System
open Microsoft.Xna.Framework.Graphics
open PalleteColor

type Snake = 
    { Head: Vector2
    ; Body: Vector2 list
    ; Tail: Vector2
    ; Heading: Heading
    ; GrowthLengthLeft: int
    }
and Heading = Axis * AxisDirection
and Axis = X | Y
and AxisDirection = Positive | Negative

let private headingUnitVector =
    function
    | (X, Positive) ->  Vector2.UnitX
    | (X, Negative) -> -Vector2.UnitX
    | (Y, Positive) ->  Vector2.UnitY
    | (Y, Negative) -> -Vector2.UnitY

let private withoutLastItem xs =
    xs
    |> List.rev 
    |> List.tail 
    |> List.rev

type PowerUp = 
    | Regular
    | Big
    | Jumbo

let powerUpGrowthLength = function
| Regular -> 5
| Big -> 10
| Jumbo -> 20

let update 
    (snake: Snake) 
    (newHeading: Heading option) 
    (newPowerUp: PowerUp option)
    (headTransport: Vector2 option)
    (gameTimeElapse: TimeSpan): Snake =
    let huv = headingUnitVector snake.Heading 
    let validNewHeading = 
        newHeading
        |> Option.bind (fun h -> 
            let newUv = headingUnitVector h
            (* The heading corresponding to a 90-degree turn (a valid turn)
            ** will have a unit vector that is *orthogonal* to the current heading's unit vector.
            ** We can determine orthogonality with Vector2.Dot *)
            if Vector2.Dot(huv, newUv) = 0.0f 
            then printfn "valid new heading!"; Some h //Head changes, heading changes, body changes
            else None)
    match validNewHeading with
    | Some h -> 
        { Head = snake.Head 
        ; Body = snake.Head :: snake.Body
        ; Tail = snake.Tail
        ; Heading = h
        ; GrowthLengthLeft = snake.GrowthLengthLeft
        }
    | None ->
        let newGrowth = newPowerUp |> Option.map powerUpGrowthLength |> Option.defaultValue 0
        let gll = snake.GrowthLengthLeft + newGrowth
        let tailPrior = 
            snake.Body 
            |> List.tryLast 
            |> Option.defaultValue (snake.Head)
        let t, b, gll_ = 
            if snake.Tail = tailPrior 
            then 
                tailPrior, snake.Body |> withoutLastItem, gll
            elif snake.GrowthLengthLeft > 0
            then
                snake.Tail, snake.Body, gll - 1
            else
                let tuv = tailPrior - snake.Tail
                tuv.Normalize()
                snake.Tail + tuv, snake.Body, gll
        { Head = snake.Head + huv 
        ; Body = b
        ; Tail = t
        ; Heading = snake.Heading
        ; GrowthLengthLeft = gll_
        }

let normalColor = Primary, Low
let growingColor = SecondaryB, High
let draw (sb: SpriteBatch) (t: Texture2D) (snake: Snake): unit =
    let vertices = snake.Head :: snake.Body @ [snake.Tail] 
    let segments = List.pairwise vertices
    let color = Color.Lerp(toColor normalColor, toColor growingColor, (float32 snake.GrowthLengthLeft) / 40.0f)
    for (a,b) in segments do
        let w = int <| Math.Abs(a.X - b.X)
        let h = int <| Math.Abs(a.Y - b.Y)
        let x = int <| Math.Min(a.X, b.X)
        let y = int <| Math.Min(a.Y, b.Y)
        let r = new Rectangle(x, y, w + Tile.size, h + Tile.size)
        sb.Draw(t, r, color) 
    ()