module Movement

open Microsoft.Xna.Framework

type Heading = Axis * AxisDirection
and Axis = X | Y
and AxisDirection = Positive | Negative

let headingToUnitVector =
    function
    | (X, Positive) ->  Vector2.UnitX
    | (X, Negative) -> -Vector2.UnitX
    | (Y, Positive) ->  Vector2.UnitY
    | (Y, Negative) -> -Vector2.UnitY