open System

let circleArea radius : float =
    let pi = 3.14159
    pi * radius * radius

let cylinderVolume radius height : float =
    let area = circleArea radius
    height * area

let rec sumDigits number : int = 
    if number = 0 then 0
    else 
        (number % 10) + sumDigits(number / 10)

[<EntryPoint>]
let main argv =
    //Объем цилиндра
    Console.WriteLine("Введите радиус цилиндра:")
    let radius = Console.ReadLine() |> float
    Console.WriteLine("Введите высоту цилиндра:")
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

    //Сумма цифр
    Console.WriteLine("Введите число, чтобы посчитать сумму его цифр:")
    let input = Console.ReadLine()
    let number = int input  

    let sum = sumDigits number
    Console.WriteLine($"Сумма цифр {number} равна {sum}")

    0
