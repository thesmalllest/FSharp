open System

let rec gcd a b =
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

let isPrime n =
    let rec check d =
        match d with
        | d when d * d > n -> true
        | d when n % d = 0 -> false
        | _ -> check (d + 1)
    match n with
    | n when n < 2 -> false
    | _ -> check 2

let getDivisors n =
    let rec loop d acc =
        match d > n with
        | true -> acc
        | false when n % d = 0 -> loop (d + 1) (d :: acc)
        | false -> loop (d + 1) acc
    loop 1 [] |> List.rev

let getPrimeDivisors n =
    getDivisors n
    |> List.filter (fun d -> isPrime d)

let rec multiply lst =
    match lst with
    | [] -> 1
    | x :: xs -> x * multiply xs

let rad n =
    getPrimeDivisors n |> multiply

let isCoprimeTriple (a, b, c) =
    gcd a b = 1 && gcd a c = 1 && gcd b c = 1

[<EntryPoint>]
let main argv = 

    printfn "%A" (rad 4320)
    printfn "%A" (isCoprimeTriple (32, 27, 5))

    0