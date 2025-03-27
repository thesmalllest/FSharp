open System

let circleArea radius : float =
    let pi = 3.14159
    pi * radius * radius

let cylinderVolume radius height : float =
    let area = circleArea radius
    height * area

//Рекурсия вверх
let rec sumDigitsUp number : int = 
    if number = 0 then 0
    else 
        (number % 10) + sumDigitsUp(number / 10)

//Рекурсия вниз
let sumDigitsDown n = 
    let rec sum n curSum = 
        if n = 0 then curSum
        else
            let n1 = n / 10
            let digit = n % 10
            let result = curSum + digit
            sum n1 result 
    sum n 0

[<EntryPoint>]
let main argv =
(*    //Объем цилиндра
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
    Console.WriteLine($"Объём цилиндра: {result}")*)

    //Сумма цифр
    Console.WriteLine("Введите число, чтобы посчитать сумму его цифр:")
    let input = Console.ReadLine()
    let number = int input  

    let sum_1 = sumDigitsUp number
    Console.WriteLine($"Сумма цифр {number} равна {sum_1}")

    let sum_2 = sumDigitsDown number
    Console.WriteLine($"Сумма цифр {number} равна {sum_2}")

    0
