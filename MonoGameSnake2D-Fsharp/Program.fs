open Game
open System

[<EntryPoint>]
let main argv = 
    let printTitle () =
        printfn @"      ______     __   __     ______     __  __     ______    "
        printfn @"     /\  ___\   /\ '-.\ \   /\  __ \   /\ \/ /    /\  ___\   "
        printfn @"     \ \___  \  \ \ \-.  \  \ \  __ \  \ \  _'-.  \ \  __\   "
        printfn @"      \/\_____\  \ \_\\'\_\  \ \_\ \_\  \ \_\ \_\  \ \_____\ "
        printfn @"       \/_____/   \/_/ \/_/   \/_/\/_/   \/_/\/_/   \/_____/ "
        printfn ""
        printfn "     Welcome to Snake!"
        printfn ""
    printTitle()
    let levelChoices =
        [ ConsoleKey.D, ("Demo",    Level.demo)
        ; ConsoleKey.W, ("Wrapper", Level.simpleWrapper)
        ; ConsoleKey.H, ("Huuuge",  Level.huuuge)
        ; ConsoleKey.S, ("Snake-permissive tile",  Level.snakePermissiveTile)
        ] 
    let levelChoicesMap = levelChoices |> Map.ofList
    let rec startGame () =
        printfn "     Please choose one of the following levels:"
        printfn ""
        for (key,(name,_)) in levelChoices do
            printfn "          [ %A ] %s" key name
        printfn "          [ESC] Exit"
        printf "           "
        let key = Console.ReadKey().Key 
        match key with
        | ConsoleKey.Escape -> 0
        | _ ->
            match levelChoicesMap |> Map.tryFind key with
            | None -> 
                printfn "     Invalid Choice."
                printfn ""
                startGame()
            | Some (name,level) -> 
                Console.Clear()
                printfn "Starting %s..." name
                let game = new Game1(level())
                game.Run()
                Console.Clear()
                printTitle()
                startGame()
    startGame()