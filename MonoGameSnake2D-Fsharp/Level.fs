module Level

open Movement
open PalleteColor

type Level = 
    { WidthInTiles: int // NOTE: Because the game uses an aspect ratio of 16:9, this number out to be a multiple of 16.
    ; Blocks: Block.Block list
    ; Snake: Snake.Snake
    ; BouncyBlocks: BouncyBlock.BouncyBlock list
    ; Wormholes: Wormhole.Wormhole list }

let private makePerimeter (x1,y1) (x2,y2): Tile.Tile list = 
    List.concat
        [
            [y1..y2] |> List.collect (fun y -> [x1,y; x2,y])
            [x1..x2] |> List.collect (fun x -> [x,y1; x,y2])
        ]

let private makePerimeterPairs (x1,y1) (x2,y2): (Tile.Tile * Tile.Tile) list =
    List.concat
        [
            [x1..x2] |> List.map (fun x -> (x,y1), (x,y2))
            [y1..y2] |> List.map (fun y -> (x1,y), (x2,y))
        ]

let demo () =
    { WidthInTiles = 80
    ; Snake = Snake.makeSnake (60,5) (X, Positive) (25) 1.25f
    ; BouncyBlocks =
        [
            BouncyBlock.make (30,20) (31,20)
            BouncyBlock.make (40,34) (40,33)
            BouncyBlock.make (50,20) (52,21)
            BouncyBlock.make (30,20) (28,21)
            BouncyBlock.make (50,15) (51,17)
            BouncyBlock.make (20,15) (21,13)
        ]
    ; Blocks =
        List.concat 
            [
                makePerimeter ( 0, 0) (79,44) //level border
                makePerimeter (20,10) (59,35) //inner cloister
            ]
    ; Wormholes = 
        [ Wormhole.makeWormhole 
            (Complement,Normal) 
            (10,5) 
            (70,40)
            (Rotate Clockwise)
        ; Wormhole.makeWormhole 
            (SecondaryB,Normal) 
            (10,40) 
            (70,5)
            (Noop)
        ; Wormhole.makeWormhole
            (SecondaryA,Lowest)
            (70,25)
            (70,43)
            (Reflect)
        ]
    }

let simpleWrapper () =
    let w = 16
    let h = (float w) / (16.0 / 9.0) |> int
    { WidthInTiles = w
    ; Snake = Snake.makeSnake (w*2/3,5) (X, Positive) (w/3) (1.0f / 4.0f)
    ; BouncyBlocks = []
    ; Blocks = [ ]
    ; Wormholes = 
        makePerimeterPairs (-1, -1) (w,h)
        |> List.map (fun (a,b) ->
            Wormhole.makeWormhole (Complement,Normal) a b Noop)
    }

let huuuge () =
    let w = 16 * 18
    let h = (float w) / (16.0 / 9.0) |> int
    { WidthInTiles = w
    ; Snake = Snake.makeSnake (w*2/3,5) (X, Positive) (w/3) 5.0f
    ; BouncyBlocks = []
    ; Blocks = [ ]
    ; Wormholes = 
        makePerimeterPairs (-1, -1) (w,h)
        |> List.map (fun (a,b) ->
            Wormhole.makeWormhole (Complement,Normal) a b Noop)
    }