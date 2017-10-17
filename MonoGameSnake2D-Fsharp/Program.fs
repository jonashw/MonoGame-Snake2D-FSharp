open Game

[<EntryPoint>]
let main argv = 
    let game = new Game1(Level.demo())
    game.Run()
    0 
