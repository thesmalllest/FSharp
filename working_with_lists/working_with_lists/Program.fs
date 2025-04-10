// task 1.7

let shiftRight list =
    match list with
    | [] -> []
    | _ -> 
        let last = List.last list
        last::(list |> List.take (List.length list - 1))

let swapMinMax list =
    let minEl = List.min list
    let maxEl = List.max list
    let rec swap l = 
        match l with
        | [] -> []
        | h::t when h = minEl -> maxEl :: (swap t)
        | h::t when h = maxEl -> minEl :: (swap t)
        | h::t -> h :: (swap t)
    swap list



[<EntryPoint>]
let main argv = 
    let l = [1; 2; 3; 4; 5]

    let result_1_7 = shiftRight (shiftRight l)
    printfn "%A" result_1_7

    let result_1_17 = swapMinMax l
    printfn "%A" result_1_17

    0
