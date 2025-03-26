open System

let circleArea radius : float =
    let pi = 3.14159
    pi * radius * radius

let cylinderVolume radius height : float =
    let area = circleArea radius
    height * area

[<EntryPoint>]
let main argv =
    Console.WriteLine("Введите радиус:")
    let radius = Console.ReadLine() |> float

    Console.WriteLine("Введите высоту:")
    let height = Console.ReadLine() |> float

    //Суперпозиция
    let countVolume (r, h) = cylinderVolume r h
    let printVolume v = Console.WriteLine($"Объём цилиндра: {v}")
    let volume = countVolume >> printVolume
    volume (radius, height)

    //Каррирование
    let volumeFunction = cylinderVolume radius
    let result = volumeFunction height
    Console.WriteLine($"Объём цилиндра: {result}")

    0
