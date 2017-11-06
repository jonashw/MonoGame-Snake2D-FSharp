module Example

open NUnit.Framework
open Snake

[<TestFixture>]
type Example() =
    let s = Snake.makeSnake (0,0) (Movement.X, Movement.Positive) 5 0.0f

    [<Test>]
    member x.``is digital by default`` () =
        Assert.True(Snake.isTileDigital s)