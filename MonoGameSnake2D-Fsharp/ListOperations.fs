module ListOperations

let tryDropLast xs =
    xs
    |> List.rev 
    |> function
       | [] -> []
       | last :: init -> List.rev init