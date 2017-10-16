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
