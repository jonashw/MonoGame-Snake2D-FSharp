module Level

open Movement
open PalleteColor

type Level = 
    { WidthInTiles: int // NOTE: Because the game uses an aspect ratio of 16:9, this number out to be a multiple of 16.
    ; Blocks: Block.Block list
    ; Snake: Snake.Snake
    ; BouncyBlocks: BouncyBlock.BouncyBlock list
    ; Wormholes: Wormhole.Wormhole list }

let private makePerimeter bp (x1,y1) (x2,y2): Block.Block list = 
    List.concat
        [
            [y1..y2] |> List.collect (fun y -> [x1,y; x2,y])
            [x1..x2] |> List.collect (fun x -> [x,y1; x,y2])
        ] 
        |> List.map (fun t -> t,bp)

let private makePerimeterPairs bp (x1,y1) (x2,y2): (Block.Block * Block.Block) list =
    List.concat
        [
            [x1..x2] |> List.map (fun x -> ((x,y1),bp), ((x,y2),bp))
            [y1..y2] |> List.map (fun y -> ((x1,y),bp), ((x2,y),bp))
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
                makePerimeter Block.NotPermissive ( 0, 0) (79,44) //level border
                makePerimeter Block.NotPermissive (20,10) (59,35) //inner cloister
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
        makePerimeterPairs Block.NotPermissive (-1, -1) (w,h)
        |> List.map (fun (a,b) ->
            Wormhole.makeWormhole (Complement,Normal) (fst a) (fst b) Noop)
    }

let huuuge () =
    let w = 16 * 18
    let h = (float w) / (16.0 / 9.0) |> int
    { WidthInTiles = w
    ; Snake = Snake.makeSnake (w*2/3,5) (X, Positive) (w/3) 5.0f
    ; BouncyBlocks = []
    ; Blocks = [ ]
    ; Wormholes = 
        makePerimeterPairs Block.NotPermissive (-1, -1) (w,h)
        |> List.map (fun (a,b) ->
            Wormhole.makeWormhole (Complement,Normal) (fst a) (fst b) Noop)
    }

let snakePermissiveTile () =
    let w = 32
    let h = (float w) / (16.0 / 9.0) |> int
    { WidthInTiles = w
    ; Snake = Snake.makeSnake (w/6,5) (X, Negative) 5 (1.0f / 4.0f)
    ; BouncyBlocks = 
        [
            BouncyBlock.make (17,5) (18,6)
            BouncyBlock.make (19,h-5) (18,h-6)
        ]
    ; Blocks = 
        [
            [0..w/3] |> List.map (fun x -> (x,h/2),     Block.NotPermissive) // left jut
            [0..w/3] |> List.map (fun x -> (w-1-x,h/2), Block.NotPermissive) // right jut
            [1..h-2] |> List.map (fun y -> (w/3+1,y),   Block.SnakeOnly) // left bouncy wall
            [1..h-2] |> List.map (fun y -> (2*w/3-1,y), Block.SnakeOnly) // right bouncy wall
            makePerimeter Block.NotPermissive ( 0, 0) (w-1,h-1) //level border
        ] |> List.concat

    ; Wormholes = 
        [
            Wormhole.makeWormhole (Complement,Normal) (1,5) (w-1-1,h-5) Noop
        ]
    }