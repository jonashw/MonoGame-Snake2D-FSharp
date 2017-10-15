module ListOperations

let dropLast xs =
    xs
    |> List.rev 
    |> List.tail 
    |> List.rev