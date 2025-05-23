open System

let rec gcd a b =
    match b with
    | 0 -> a
    | _ -> gcd b (a % b)

[<EntryPoint>]
let main argv = 

    Console.WriteLine(gcd 5 27)
    Console.WriteLine(gcd 3 27)

    0