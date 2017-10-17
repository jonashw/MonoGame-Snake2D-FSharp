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
    { From: Vector2
    ; To: Vector2
    ; HeadingTransform: HeadingTransform
    }

let otherAxis = function
| X -> Y
| Y -> X

let reverse = function
| Positive -> Negative
| Negative -> Positive

let headingToUnitVector =
    function
    | (X, Positive) ->  Vector2.UnitX
    | (X, Negative) -> -Vector2.UnitX
    | (Y, Positive) ->  Vector2.UnitY
    | (Y, Negative) -> -Vector2.UnitY

let transformHeading t (axis,axisDirection as h) = 
    match t with
    | Noop -> h
    | Reflect -> 
        let newAxisDirection =
            match axisDirection with 
            | Positive -> Negative 
            | Negative -> Positive
        axis, newAxisDirection
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

let forwardEdge (rect: Rectangle) : Axis * AxisDirection -> int = function
    | X, Negative -> rect.Left
    | X, Positive -> rect.Right
    | Y, Negative -> rect.Top
    | Y, Positive -> rect.Bottom

let vectorComponent (velocity: Vector2): Axis -> float32 = function
| X -> velocity.X
| Y -> velocity.Y

let vectorAddComponent (v: Vector2) (heading: Heading) (amount: float32) =
    match heading with
    | X,Positive -> Vector2(v.X + amount, v.Y)
    | X,Negative -> Vector2(v.X - amount, v.Y)
    | Y,Positive -> Vector2(v.X, v.Y + amount)
    | Y,Negative -> Vector2(v.X, v.Y - amount)

let reflectVelocity axis (currentVelocity: Vector2) =
    match axis with 
    | X -> new Vector2(-currentVelocity.X,  currentVelocity.Y)
    | Y -> new Vector2( currentVelocity.X, -currentVelocity.Y)