module Example

open NUnit.Framework
open FsUnit
open Snake

[<TestFixture>]
type Example() =
    let s = Snake.makeSnake (0,0) (Movement.X, Movement.Positive) 5 0.0f
    [<Test>]
    member x.``is digital by default`` () =
        Snake.isTileDigital s |> should equal true