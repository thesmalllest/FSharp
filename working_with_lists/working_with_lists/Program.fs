// task 1.7
let shiftRight list =
    match list with
    | [] -> []
    | _ -> 
        let last = List.last list
        last::(list |> List.take (List.length list - 1))


// task 1.17
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

// task 1.27
let shiftLeft list =
    match list with
    | [] -> []
    | h::t -> t @ [h]

// task 18.7
let f18 list1 list2 = List.filter (fun x -> List.contains x list2) list1 

[<EntryPoint>]
let main argv = 
    let l = [1; 2; 3; 4; 5]

    let result_1_7 = shiftRight (shiftRight l)
    printfn "%A" result_1_7

    let result_1_17 = swapMinMax l
    printfn "%A" result_1_17

    let result_1_27 = shiftLeft l
    printfn "%A" result_1_27

    let list_1 = [1; 2; 3; 4; 5]
    let list_2 = [3; 4; 7]

    let result_18_7 = f18 list_1 list_2
    printfn "%A" result_18_7

    0
