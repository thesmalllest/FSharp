// task 1.7

let shiftRight list =
    match list with
    | [] -> []
    | _ -> 
        let last = List.last list
        last::(list |> List.take (List.length list - 1))

[<EntryPoint>]
let main argv = 
    let l = [1; 2; 3; 4; 5]

    let result_1_7 = shiftRight (shiftRight l)
    printfn "%A" result_1_7

    0
