module Movement

open Microsoft.Xna.Framework

type Heading = Axis * AxisDirection
and Axis = X | Y
and AxisDirection = Positive | Negative 
and HeadingTransform =
    | Noop
    | Reflect
    | Rotate of CircularDirection
and CircularDirection = Clockwise | CounterClockwise
type Teleport = 
    { To: Rectangle
    ; HeadingTransform: HeadingTransform
    }

let orthogonal (a: Heading) (b: Heading) =
    (fst a) <> (fst b)

let reflect ((axis,direction) : Heading): Heading =
    let newDirection =
        match direction with 
        | Positive -> Negative 
        | Negative -> Positive
    axis, newDirection

type Axis with
    member x.Other = 
        match x with 
        | X -> Y 
        | Y -> X

type AxisDirection with
    member x.Inverse = 
        match x with 
        | Positive -> Negative 
        | Negative -> Positive

type Microsoft.Xna.Framework.Vector2 with
    member v.Component = 
        function 
        | X -> v.X 
        | Y -> v.Y
    member v.ComponentDirection = 
        v.Component 
        >> (fun n -> if n >= 0.0f 
                     then Positive 
                     else Negative)
    member v.AddComponent  (amount: float32): Heading -> Vector2 =
        function
        | X,Positive -> Vector2(v.X + amount, v.Y)
        | X,Negative -> Vector2(v.X - amount, v.Y)
        | Y,Positive -> Vector2(v.X, v.Y + amount)
        | Y,Negative -> Vector2(v.X, v.Y - amount)
    member v.Reflect axis =
        match axis with 
        | X -> new Vector2(-v.X,  v.Y)
        | Y -> new Vector2( v.X, -v.Y)

let headingToUnitVector =
    function
    | (X, Positive) ->  Vector2.UnitX
    | (X, Negative) -> -Vector2.UnitX
    | (Y, Positive) ->  Vector2.UnitY
    | (Y, Negative) -> -Vector2.UnitY

let transformHeading t (axis,axisDirection as h) = 
    match t with
    | Noop -> h
    | Reflect -> reflect h
    | Rotate d -> 
        (*
               Y-
            X-    X+
               Y+
        *)
        match axis, axisDirection, d with
        | X, Negative, Clockwise -> Y, Negative
        | Y, Negative, Clockwise -> X, Positive
        | X, Positive, Clockwise -> Y, Positive
        | Y, Positive, Clockwise -> X, Negative
        | X, Negative, CounterClockwise -> Y, Positive
        | Y, Positive, CounterClockwise -> X, Positive
        | X, Positive, CounterClockwise -> Y, Negative
        | Y, Negative, CounterClockwise -> X, Negative

type Microsoft.Xna.Framework.Rectangle with
    member r.Edge (heading: Heading) =
        match heading with
        | X, Negative -> r.Left
        | X, Positive -> r.Right
        | Y, Negative -> r.Top
        | Y, Positive -> r.Bottom
    member r.Min = Vector2(r.Left |> float32, r.Top |> float32)
    member r.Max = Vector2(r.Right |> float32, r.Bottom |> float32)

type RectangleF with
    member r.Edge (heading: Heading) =
        match heading with
        | X, Negative -> r.Left
        | X, Positive -> r.Right
        | Y, Negative -> r.Top
        | Y, Positive -> r.Bottom
    member r.Corner (heading: Heading) =
        match heading with
        | X, Negative -> Vector2(r.Left, r.Top)
        | X, Positive -> Vector2(r.Right, r.Top)
        | Y, Negative -> Vector2(r.Left, r.Top)
        | Y, Positive -> Vector2(r.Left, r.Bottom)

let reflectVelocity axis (currentVelocity: Vector2) =
    match axis with 
    | X -> new Vector2(-currentVelocity.X,  currentVelocity.Y)
    | Y -> new Vector2( currentVelocity.X, -currentVelocity.Y)