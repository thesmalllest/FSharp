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

//Количество делителей Вверх
let rec countDevisiorsUp x index : int =
    match index with
    | index when (x % index = 0 && index < x) -> 1 + countDevisiorsUp x (index + 1)
    | index when (index >= x) -> 0
    | _ -> 0 + countDevisiorsUp x (index + 1)

//Количество делителей Вниз
let countDevisiorsDown x = 
    let rec countDevisiorsDown x index sum =
        let isNeed = (x % index = 0) && (index < x)
        let new_sum = sum + 1
        match isNeed with
            | true -> countDevisiorsDown x (index + 1) (new_sum)
            | false when index < x -> countDevisiorsDown x (index + 1) sum
            | _ -> sum
    countDevisiorsDown x 1 0

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
    let input_for_sum = Console.ReadLine()
    let number_for_sum = int input_for_sum  

    let sum_1 = sumDigitsUp number_for_sum
    Console.WriteLine($"Сумма цифр {number_for_sum} равна {sum_1}")

    let sum_2 = sumDigitsDown number_for_sum
    Console.WriteLine($"Сумма цифр {number_for_sum} равна {sum_2}")


    Console.WriteLine("Введите число, чтобы посчитать количество делителей:")
    let input_for_count = Console.ReadLine()
    let number_for_count = int input_for_count

    let count_1 = countDevisiorsUp number_for_count 1
    Console.WriteLine($"Количество делителей {number_for_count}: {count_1}")

    let count_2 = countDevisiorsDown number_for_count
    Console.WriteLine($"Количество делителей {number_for_count}: {count_2}")

    0







